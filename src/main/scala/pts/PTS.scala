package pts

case class PTS(axioms: PTS.Axioms, rules: PTS.Rules)

object PTS {
  type Axioms = Map[String, String]
  type Rules = Map[(String, String), String]
}
