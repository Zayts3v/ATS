/**
 * Classe responsável por enviar uma excepção, caso o veículo
 * já exista na aplicação. Esta invoca a super classe exception 
 * e devolve uma exception para o ecrã.
 * 
 * 
 * 
 * 
 * @version 20190415
 */

public class VeiculoJaExisteException extends Exception{
    public VeiculoJaExisteException (String s){
        super(s);
    }
}