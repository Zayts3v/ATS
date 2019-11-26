/**
 * Classe que representa um proprietário da aplicação.
 * 
 * 
 * 
 * 
 * @version 20190415
 */

import java.util.stream.Collectors;
import java.util.GregorianCalendar;
import java.util.ArrayList;
import java.util.List;

public class Proprietario extends Utilizador{   
  
    /**
     * Construtor por omissão.
     * É criada uma instância da classe Proprietario.
     */
    public Proprietario (){
        super();
    }
    
    /**
     * Construtor por parâmetro.
     * @param nome            Nome do proprietário.
     * @param nif             NIF do proprietário.
     * @param email           Email do proprietário.
     * @param password        Password do proprietário.
     * @param morada          Morada do proprietário.
     * @param dataNascimento  Data de nascimento do proprietário.
     */
    public Proprietario(String nome, String nif, String email, String password, String morada, GregorianCalendar dataNascimento){
        super(nome, nif, email,  password, morada, dataNascimento);
    }
    
    /**
     * Construtor por cópia.
     * @param outroProp Um outro proprietário.
     */
    public Proprietario (Proprietario outroProp){
        super(outroProp);
    }
     
    /**
     * Compara a igualdade com outro objeto.
     * @param obj O objeto a comparar.
     * @return true se forem iguais, false caso contrário.
     */
    public boolean equals(Object obj) {
        if(obj==this) return true; 
        
        if(obj==null || obj.getClass()!= this.getClass()) return false;
        
        Proprietario prop = (Proprietario) obj;
        return super.equals(prop);
    }
    
    /** 
     * Devolve uma representação do objeto em formato textual.
     * @return String Representa o objeto em formato textual.
     */
    public String toString(){
       return super.toString();
    }
    
    /**
     * Retorna uma cópia da instância.
     * @return Proprietario Um novo proprietário que é cópia do this.
     */
    public Proprietario clone(){
        return new Proprietario(this);
    }   
}
    
    