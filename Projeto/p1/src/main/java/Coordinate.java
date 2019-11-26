/**
 * Esta classe representa a latitude e longitude 
 * geometricamente num plano 2D.
 *   
 * 
 * 
 * 
 */

import java.io.Serializable;
import java.text.DecimalFormat;

public class Coordinate implements Comparable<Coordinate>{
    
  private double latitude;
  private double longitude;
  private DecimalFormat format;
  
  /**
   * Construtor parametrizado
   */
  public Coordinate(double latitude, double longitude) {
      this.latitude = latitude;
      this.longitude = longitude;
      this.format = new DecimalFormat("##.######");
  }

  public Coordinate(Coordinate outro) {
      this.latitude = outro.getLatitude();
      this.longitude = outro.getLongitude();
      this.format = new DecimalFormat("##.######");
  }
  
  /*****************************************************************************
  *                             BEGIN GETTERS                                 *
  *****************************************************************************/
  
  public double getLatitude() {
    return latitude;
  }
  
  public double getLongitude() {
    return longitude;
  }
  
  /*****************************************************************************
  *                              END GETTERS                                  *
  *****************************************************************************/
     
  /*****************************************************************************
  *                             BEGIN SETTERS                                 *
  *****************************************************************************/
  
  public void setLatitude(double latitude) {
    if(CoordinateManager.isValidLatitude(latitude) == true) {
      this.latitude = latitude;
    } else {
      throw new IllegalArgumentException("The parameter did not pass validation as defined by the CoordinateManager class");
    }
  }
  
  public void setLongitude(double longitude) {
    if(CoordinateManager.isValidLongitude(longitude) == true) {  
      this.longitude = longitude;
    } else {
      throw new IllegalArgumentException("The parameter did not pass validation as defined by the CoordinateManager class");
    }
  }
  
  /*****************************************************************************
  *                              END SETTERS                                  *
  *****************************************************************************/  

  /**
   * Método que calcula a distância entre duas coordenadas.
   * @param a Coordenada a calcular a distância entre a do this.
   * @return double Distância entre as duas coordenadas.
   */
  public double getDistancia(Coordinate a){
      return Math.sqrt(Math.pow((a.getLatitude() - this.latitude), 2) - Math.pow((a.getLongitude() - this.longitude), 2));
  }
  
  /**
  * Redefinição do compareTo para fazer a comparação de duas coordenadas, e comparar a distância de cada uma.
  * @param b Coordenada a comparar.
  * @return int -1 se a distância do this for menor do que a distância das coordenadas recebidas como parâmetro, 
  *              1 se for ao contrário e 0 caso sejam iguais.
  */
  public int compareTo(Coordinate b){
      if(getDistancia(this) < getDistancia(b)){
            return -1;
      }
      if(getDistancia(b) < getDistancia(this)){
            return 1;
      }
      return 0;
  }
  
  public String getLatitudeAsString() {
  
    return format.format(latitude);
  }
  
  public String getLongitudeAsString() {
    return format.format(longitude);
  }
  
  public String toString() {
    return format.format(latitude) + ", " + format.format(longitude);
  }
  
  
  public boolean equals(Object o) {
      // check to make sure the object is an event
      if ((o instanceof Coordinate) == false) {
          // o is not an event object
       return false;
    }
    
    // compare these two events
    Coordinate c = (Coordinate)o;
    
    // build items for comparison
    String me  = this.getLatitudeAsString() + this.getLongitudeAsString();
    String you = c.getLatitudeAsString() + c.getLongitudeAsString();
    
    return me.equals(you);
    
  }
  
  public Coordinate clone(){
      return new Coordinate(this);
  }
}
