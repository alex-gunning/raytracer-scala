import scala.math.pow

object Lighting {
  case class PointLight(position: Point, intensity: Colour)

  def lighting(material: Material,
               light: PointLight,
               point: Point,
               eye: Vector,
               normal: Vector): Colour = {
    val effectiveColour = material.colour * light.intensity
    val lightv = (light.position - point).normalized()
    val lightDotNormal = lightv.dot(normal)
    val ambient = effectiveColour * material.ambient
    var diffuse = Colour(0,0,0)
    var specular = Colour(0,0,0)

    if(lightDotNormal < 0) {
      diffuse = Colour(0, 0, 0)
      specular = Colour(0, 0, 0)
    } else {
      diffuse = effectiveColour * material.diffuse * lightDotNormal
      val reflectv = (-lightv).reflect(normal)
      val reflectDotEye = reflectv.dot(eye)
      if(reflectDotEye <= 0) {
        specular = Colour(0,0,0)
      } else {
        val factor = pow(reflectDotEye, material.shininess).toFloat
        specular = light.intensity * material.specular * factor
      }
    }
    ambient + diffuse + specular
  }
}