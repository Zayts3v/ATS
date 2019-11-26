/**
 * Classe responsável por enviar uma excepção, caso o veículo
 * não pertença ao proprietário atual. Esta invoca a super classe 
 * exception e devolve uma exception para o ecrã.
 * 
 * 
 * 
 * 
 * @version 20190415
 */

public class VeiculoNaoESeuException extends Exception{
    public VeiculoNaoESeuException (String s){
        super(s);
    }
}
