/**
 * Classe responsável por enviar uma excepção, caso o utilizador
 * não exista. Esta invoca a super classe exception e devolve uma
 * exception para o ecrã.
 * 
 * 
 * 
 * 
 * @version 20190415
 */

public class UtilizadorNaoExisteException extends Exception{
    public UtilizadorNaoExisteException (String s){
        super(s);
    }
}
