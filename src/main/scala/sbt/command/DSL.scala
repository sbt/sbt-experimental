package sbt.command

import sbt.{State => _, _}


/**
 * DSL for creating sbt commands.
 */
object DSL {
  type SbtState[A] = _root_.cats.state.State[_root_.sbt.State, A]

  import cats.std.all._
  import cats.state.{State=>CatsState, _}


  /** The current sbt Session.  Allows you to add settings, save settings, etc. */
  def session: SbtState[sbt.SessionSettings] = CatsState { state => state -> Project.session(state) }
  def extractedProject: SbtState[sbt.Extracted] = CatsState { state => (state, Project.extract(state))}
  def currentProject: SbtState[ResolvedProject] = extractedProject.map(_.currentProject)
  def currentRef: SbtState[ProjectRef] = extractedProject.map(_.currentRef)
  def currentUnit: SbtState[LoadedBuildUnit] = extractedProject.map(_.currentUnit)

  /** Runs a task using the current state. */
  def runTask[A](key: TaskKey[A]): SbtState[A] =
     CatsState { state =>
       val extracted = Project.extract(state)
       // TODO - figure out if we can "transform" the scope of the task key for the current project/ref.
       extracted.runTask(key, state)
     }

  /** Returns the setting value for a given setting. */
  def getSetting[A](key: SettingKey[A]): SbtState[A] =
    CatsState { state =>
      val extracted = Project.extract(state)
      state -> extracted.get(key)
    }

  /** Returns the setting value for a given setting or None */
  def getSettingOpt[A](key: SettingKey[A]): SbtState[Option[A]] =
    CatsState { state =>
      val extracted = Project.extract(state)
      state -> extracted.getOpt(key)
    }

  /** Runs a given input task with the given string input. */
  def runInputTask[A](key: InputKey[A], input: String): SbtState[A] =
    CatsState { state =>
      val extracted = Project.extract(state)
      extracted.runInputTask(key, input, state)
    }

  /** Runs the given command string through state. */
  def runCommand(cmd: String): SbtState[Unit] =
    CatsState { state =>
      Command.process(cmd, state) -> ()
    }

  /** Transforms a set of settings as if the they are coming from the build.sbt file of the current project,
    * as defined by the current session of sbt.
    * @param settings
    * @return
    */
  def transformSettings(settings: Seq[Setting[_]]): SbtState[Seq[Setting[_]]] =
    for {
      extracted <- extractedProject
      proj <- currentRef
    } yield Load.transformSettings(Load.projectScope(proj), proj.build, extracted.rootProject, settings)

  /** Appends a sequence of settings to the current sbt session, and reloads the build. */
  def appendSessionRaw(appendSettings: Seq[Setting[_]]): SbtState[Unit] =
    CatsState { state =>
      // reloads with appended settings.
      val session = Project.session(state)
      //val structure = Project.structure(state)
      //implicit val display = Project.showContextKey(state)
      // When we reload, make sure we keep all reapplied settings...
      //val newStructure = Load.reapply(session.mergeSettings ++ appendSettings, structure)
      val newSession = session.appendRaw(appendSettings)
      // updates various aspects of State based on the new settings
      // and returns the updated State
      SessionSettings.reapply(newSession, state) -> ()
    }

  /** Appends a set of settings to the current sbt session, first performing any scope translations as they would
    * apply to a build.sbt for the current "main" project in scope.
    * @param appendSettings
    * @return
    */
  def appendSessionRawTransformed(appendSettings: Seq[Setting[_]]): SbtState[Unit] =
    for {
      transformed <- transformSettings(appendSettings)
      result <- appendSessionRaw(transformed)
    } yield result


  /** Create an "action" for a simple sbt.Command. */
  private def action[A](process: SbtState[A]): sbt.State => sbt.State = { state => process.run(state).run._1 }
  /** Convert a simple workflow to construct a parser into a new parser. TODO - prevent parsers from using non-read-only methods on State. */
  private def parser[A](parser: SbtState[sbt.complete.Parser[A]]): sbt.State => sbt.complete.Parser[A] = { state => parser.run(state).run._2 }
  /** Advanced command generator.  Allows you to create a parser for strings and use the results to generate a new command workflow. */
  def command[A,U](name: String, help: Help)(p: SbtState[sbt.complete.Parser[A]])(a: A => SbtState[U]): sbt.Command = {
    Command.apply(name, help)(parser(p)) { (state, parsed) =>
      action(a(parsed))(state)
    }
  }
  /** Creates a simple sbt command from a workflow. */
  def simpleCommand[A](name: String)(action: SbtState[A]): sbt.Command = Command.command(name)(DSL.action(action))

  /** Runs a given sbt-state process using some sbt state and returns the next state. */
  def run[A](process: SbtState[A], state: sbt.State): (sbt.State, A) = process.run(state).run
}
