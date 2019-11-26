/**
 * Classe que representa um utilizador da aplicação.
 * 
 * 
 * 
 * 
 * @version 20190415
 */

import java.io.Serializable;
import java.util.GregorianCalendar;

public class Utilizador implements Serializable {
    
    /** Variáveis de Instância */
    
    /* Email (nome do utilizador) */
    private String email; 
    /* Nome */
    private String nome; 
    /* NIF */
    private String nif;
    /* Password */
    private String password;
    /* Morada */
    private String morada; 
    /* Data de Nascimento */ 
    private GregorianCalendar dataNascimento; 
    
    /**
     * Construtor por omissão.
     * É criada uma instância da classe Utilizador.
     */
    public Utilizador(){
        this.email = "";
        this.nome = "";
        this.nif = "";
        this.password = "";
        this.morada = "";
        this.dataNascimento = new GregorianCalendar();
    }
    
    /**
     * Construtor por parâmetro.
     * @param nome            Nome do utilizador.
     * @param nif             NIF do utilizador.
     * @param email           Email do utilizador.
     * @param password        Password do utilizador.
     * @param morada          Morada do utilizador.
     * @param dataNascimento  Data de nascimento do utilizador.
     */
    public Utilizador(String nome, String nif, String email, String password, String morada, 
                      GregorianCalendar dataNascimento){
        this.nome = nome;
        this.nif = nif;
        this.email =email;
        this.password = password;
        this.morada = morada;
        this.dataNascimento = dataNascimento;
    }
    
    /**
     * Construtor por cópia.
     * @param outroUser Um outro utilizador.
     */
    public Utilizador(Utilizador outroUser){
        this.nome = outroUser.getNome();
        this.nif = outroUser.getNIF();
        this.email = outroUser.getEmail();
        this.password = outroUser.getPassword();
        this.morada = outroUser.getMorada();
        this.dataNascimento = outroUser.getDataNasc();
    }
    
    /*****************************************************************************
     *                             BEGIN GETTERS                                 *
     *****************************************************************************/
    
    /**
     * Devolve o nome do utilizador.
     * @return String Representa o nome do utilizador.
     */
    public String getNome(){return this.nome;}
    
    /**
     * Devolve o NIF do utilizador.
     * @return String Representa o NIF do utilizador. 
     */
    public String getNIF(){return this.nif;}
    
    /**
     * Devolve o email do utilizador.
     * @return String Representa o email do utilizador.
     */
    public String getEmail(){return this.email;}
        
    /**
     * Devolve a password do utilizador.
     * @return String Representa a password do utilizador.
     */
    public String getPassword(){return this.password;}
    
    /**
     * Devolve a morada do utilizador.
     * @return String Representa a morada do utilizador.
     */
    public String getMorada(){return this.morada;}
    
    /**
     * Devolve a data de nascimento do utilizador.
     * @return GregorianCalendar Representa a data de nascimento do utilizador.
     */
    public GregorianCalendar getDataNasc(){return this.dataNascimento;}
    
    /*****************************************************************************
     *                              END GETTERS                                  *
     *****************************************************************************/
    
    /*****************************************************************************
     *                             BEGIN SETTERS                                 *
     *****************************************************************************/
    
    /**
     * Altera o valor do campo nome do utilizador.
     */
    public void setNome(String nome){this.nome = nome;}
    
    /**
     * Altera o valor do campo nome do utilizador.
     */
    public void setNIF(String nif){this.nif = nif;}
    
    /**
     * Altera o valor do campo email do utilizador.
     */
    public void setEmail(String email){this.email = email;}    
    
    /**
     * Altera o valor do campo password do utilizador.
     */
    public void setPassword(String password){this.password = password;}
    
    /**
     * Altera o valor do campo morada do utilizador.
     */
    public void setMorada(String morada){this.morada = morada;}
    
    /**
     * Altera o valor do campo data de nascimento do utilizador.
     */
    public void setDataNasc(GregorianCalendar dataNasc){this.dataNascimento = dataNasc;}
    
    /*****************************************************************************
     *                              END SETTERS                                  *
     *****************************************************************************/
     
    /**
     * Compara a igualdade com outro objeto.
     * @param obj O objeto a comparar.
     * @return true se forem iguais, false caso contrário.
     */
    public boolean equals(Object obj) {
        if(obj==this) return true;
        if(obj==null || obj.getClass()!=this.getClass()) return false;
        Utilizador user = (Utilizador) obj;
        return this.nome.equals(user.getNome()) && this.nif.equals(user.getNIF()) && this.email.equals(user.getEmail()) 
            && this.password.equals(user.getPassword()) && this.morada.equals(user.getMorada()) 
            && this.dataNascimento.equals(user.getDataNasc());
    }
    
    /** 
     * Devolve uma representação do objeto em formato textual.
     * @return String Representa o objeto em formato textual.
     */
    public String toString() {
       StringBuilder str = new StringBuilder();
       
       str.append("*****    UTILIZADOR    *****\n"); 
       str.append("Nome: "); str.append(this.nome); str.append("\n");
       str.append("NIF: "); str.append(this.nif); str.append("\n");
       str.append("Email: "); str.append(this.email); str.append("\n");
       str.append("Password: "); str.append(this.password); str.append("\n");
       str.append("Morada: "); str.append(this.morada); str.append("\n");
       str.append("Data de Nascimento: "); str.append(this.dataNascimento.get(GregorianCalendar.DAY_OF_MONTH));
       str.append("/"); str.append(this.dataNascimento.get(GregorianCalendar.MONTH) + 1); str.append("/"); 
       str.append(this.dataNascimento.get(GregorianCalendar.YEAR)); str.append("\n");
        
       return str.toString();
    }
    
    /**
     * Retorna uma cópia da instância.
     * @return Utilizador Um novo utilizador que é cópia do this.
     */
    public Utilizador clone(){
        return new Utilizador(this);
    }  
}
















