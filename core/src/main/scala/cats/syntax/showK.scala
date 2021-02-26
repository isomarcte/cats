package cats
package syntax

trait ShowKSyntax extends ShowK.ToShowKOps {
  implicit final def showKInterpolator(sc: StringContext): ShowK.ShowKInterpolator = ShowK.ShowKInterpolator(sc)
}
