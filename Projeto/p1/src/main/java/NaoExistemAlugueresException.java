/**
 * Classe responsável por enviar uma excepção, caso não existam alugueres
 * de um proprietário.
 *
 * 
 * 
 * 
 * @version 20190415
 */

public class NaoExistemAlugueresException extends  Exception{
    public NaoExistemAlugueresException(String s){
        super(s);
    }
}
