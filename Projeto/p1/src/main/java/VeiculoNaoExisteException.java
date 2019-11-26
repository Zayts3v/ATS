/**
 * Classe responsável por enviar uma excepção, caso o veículo
 * não exista na aplicação. Esta invoca a super classe exception 
 * e devolve uma exception para o ecrã.
 * 
 * 
 * 
 * 
 * @version 20190415
 */

public class VeiculoNaoExisteException extends Exception{
    public VeiculoNaoExisteException (String s){
        super(s);
    }
}