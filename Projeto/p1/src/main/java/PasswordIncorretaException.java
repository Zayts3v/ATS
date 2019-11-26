/**
 * Classe responsável por enviar uma excepção, caso a password
 * do utilizador esteja incorreta. Esta invoca a super classe exception 
 * e devolve uma exception para o ecrã.
 * 
 * 
 * 
 * 
 * @version 20190415
 */

public class PasswordIncorretaException extends Exception{
    public PasswordIncorretaException (String s){
        super(s);
    }
}
