/**
 * Classe que representa um cliente da aplicação.
 * 
 */

import java.util.GregorianCalendar;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

public class Cliente extends Utilizador implements Classificação{
    
    /* Posição em que se encontra o cliente. */
    private Coordinate posicao;
    /* Classificação do cliente */
    private int classificacao;
    /* Número de alugueres efetuados */
    private int nAlugueres;
    /* Número de Km totais percorridos */
    private double nKM;
    
    
    /**
     * Construtor por omissão.
     * É criada uma instância da classe Cliente.
     */
    public Cliente (){
        super();
        this.posicao = new Coordinate(0.0,0.0);
        this.classificacao = 0;
        this.nAlugueres = 0;
        this.nKM = 0.0;
    }
    
    /**
     * Construtor por parâmetro.
     * @param nome            Nome do cliente.
     * @param nif             Nif do cliente.
     * @param email           Email do cliente.
     * @param password        Password do cliente.
     * @param morada          Morada do cliente.
     * @param dataNascimento  Data de nascimento do cliente.
     * @param cords           Posição em que o cliente se encontra.
     * @param classificacao   Classificação do cliente.
     * @param nAlugs          Número de alugueres do cliente.
     * @param km              Número de Km percorridos pelo cliente.
     */
    public Cliente(String nome, String nif, String email, String password, String morada, GregorianCalendar dataNascimento, 
                   Coordinate cords, int classificacao, int nAlugs, double km){
        super(nome, nif, email, password, morada, dataNascimento);
        this.posicao = cords;
        this.classificacao = classificacao;
        this.nAlugueres = nAlugs;
        this.nKM = km;
    }
    
    /**
     * Construtor por cópia.
     * @param outroCli Um outro cliente.
     */
    public Cliente(Cliente outroCli){
        super(outroCli); 
        this.posicao = outroCli.getPosicao();
        this.classificacao = outroCli.getClassificacao();
        this.nAlugueres = outroCli.getNAlugueres();
        this.nKM = outroCli.getNKm();
    }
    
    /*****************************************************************************
    *                             BEGIN GETTERS                                 *
    *****************************************************************************/
    
    /**
     * Devolve a posição em que o cliente se encontra.
     * @return Coordinate Representa a posição (latitude e longitude) em que o cliente se encontra.
     */
    public Coordinate getPosicao(){return this.posicao.clone();}
    
    /**
     * Devolve a classificação do cliente.
     * @return int Representa a classificação do cliente.
     */
    public int getClassificacao(){return this.classificacao;}
    
    /**
     * Devolve o número de alugueres do cliente.
     * @return int Representa o número de alugueres do cliente.
     */
    public int getNAlugueres(){return this.nAlugueres;}
    
    /**
     * Devolve o número total de KM percorridos pelo cliente.
     * @return double Representa o número total de KM percorridos pelo cliente.
     */
    public double getNKm(){return this.nKM;}
   
    /*****************************************************************************
     *                              END GETTERS                                  *
     *****************************************************************************/
     
    /*****************************************************************************
     *                             BEGIN SETTERS                                 *
     *****************************************************************************/
    
    /**
     * Altera o valor do campo posição.
     * @param novaP Nova posição do cliente.
     */
    public void setPosicao(Coordinate novaP){this.posicao = novaP;}
    
    /**
     * Altera o valor do campo classificação.
     * @param classificacao Nova classificação do cliente.
     */
    public void setClassificacao(int classificacao){this.classificacao = classificacao;}
    
    /**
     * Altera o valor do campo número de alugueres do cliente.
     * @param nAlug Novo número de alugueres.
     */
    public void setNAlugueres(int nAlug){this.nAlugueres = nAlug;}
    
    /**
     * Altera o valor do campo do número total de KM percorridos pelo cliente.
     * @param km Novo número total de KM percorridos pelo cliente.
     */
    public void setNKM(double km){this.nKM = km;}
    
   /*****************************************************************************
    *                              END SETTERS                                  *
    *****************************************************************************/
    
    /**
     * Compara a igualdade com outro objeto.
     * @param obj O objeto a comparar.
     * @return true se forem iguais, false caso contrário.
     */
    public boolean equals(Object obj){
        if(obj==this) return true;
        
        if(obj==null || obj.getClass()!=this.getClass()) return false;
        
        Cliente cli = (Cliente) obj;
        return  super.equals(cli) && this.posicao.equals(cli.getPosicao()) &&
                this.classificacao == cli.getClassificacao() && this.nAlugueres == cli.getNAlugueres() &&
                this.nKM == cli.getNKm();

    }
    
    /** 
     * Devolve uma representação do objeto em formato textual.
     * @return String Representa o objeto em formato textual.
     */
    public String toString() {
       StringBuilder str = new StringBuilder();
       str.append(super.toString());
       str.append("Posição em que o cliente se encontra: "); str.append(this.posicao.toString()); str.append("\n");
       str.append("Classificação do cliente: "); str.append(this.classificacao); str.append("\n");
       str.append("Número de alugueres efetuados: "); str.append(this.nAlugueres); str.append("\n");
       str.append("Número de Km percorridos: "); str.append(this.nKM); str.append("\n");
       return str.toString();
    }
    
    /**
     * Retorna uma cópia da instância.
     * @return Cliente Um novo cliente que é cópia do this.
     */
    public Cliente clone(){
        return new Cliente(this);
    } 
}
