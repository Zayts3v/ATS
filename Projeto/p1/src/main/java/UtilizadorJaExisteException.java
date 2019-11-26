/**
 * Classe responsável por enviar uma excepção, caso o utilizador
 * já esteja registado na aplicação. Esta invoca a super classe exception 
 * e devolve uma exception para o ecrã.
 * 
 * 
 * 
 * 
 * @version 20190415
 */

public class UtilizadorJaExisteException extends Exception{
    public UtilizadorJaExisteException(String s){
        super(s);
    }
}
