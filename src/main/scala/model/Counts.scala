package model

case class Counts(classNo: Int, objectNo: Int, traitNo: Int, packageObjNo: Int) {
  def incClassNo      = this.copy(classNo      = this.classNo + 1)
  def incObjectNo     = this.copy(objectNo     = this.objectNo + 1)
  def incTraitNo      = this.copy(traitNo      = this.traitNo + 1)
  def incPackageObjNo = this.copy(packageObjNo = this.packageObjNo + 1)
}

object Counts {
  val initial = Counts(0, 0, 0 ,0)
}
