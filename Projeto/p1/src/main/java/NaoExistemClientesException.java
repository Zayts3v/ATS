/**
 * Classe responsável por enviar uma excepção, caso não existam
 * clientes. Esta invoca a super classe exception e devolve uma
 * exception para o ecrã.
 *
 * 
 * 
 * 
 * @version 20190415
 */

public class NaoExistemClientesException extends Exception{
    public NaoExistemClientesException (String s){
        super(s);
    }
}