    /**
     * Classe que representa um aluguer na aplicação.
     * 
     */
    
    import java.io.Serializable;
    import java.util.GregorianCalendar;

public class Aluguer implements Serializable {
    
    /* Email do cliente que requisitou o aluguer */
    private String mail;
    /* Matrícula do veículo alugado */
    private String matricula;
    /* Data de início do aluguer */
    private GregorianCalendar dataInicio;
    /* Data de fim do aluguer */
    private GregorianCalendar dataFim;
    /* Custo da viagem efetuada */
    private double custoViagem;
    /* Tempo que o cliente demora a chegar a pé ao veiculo (em minutos) */
    private int tempoAteVeiculo;
    /* Tempo total da viagem em minutos */
    private int tempoViagem;
    /* Destino do Cliente */
    private Coordinate destino;
    /* Número de Kms desde a origem até ao destino */
    private double distancia;
    /* Aluguer foi aceite pelo proprietário ou não */
    private boolean aceite;
    /* Aluguer está em lista de espera ou não */
    private boolean listaEspera;
    /* Aluguer já foi realizado ou não */
    private  boolean realizado;
    /* Aluguer foi alterado de preço ou não */
    private boolean altPrice;
    /* Inteiro que representa o estado da classificação, 0 - não foi efetuada nenhuma classificação,
       1 - classificação dada apenas ao cliente, 2 - classificação dada apenas ao veículo,
       3 - classificação foi dada a ambos */
    private int estadoClassif;

    /**
     * Construtor por omissão.
     * É criada uma instância da classe Aluguer.
     */
    public Aluguer(){
        this.mail = "";
        this.matricula = "";
        this.dataInicio = new GregorianCalendar();
        this.dataFim = new GregorianCalendar();
        this.custoViagem = 0.0;
        this.tempoAteVeiculo = 0;
        this.tempoViagem = 0;
        this.destino = new Coordinate(0.0, 0.0);
        this.distancia = 0.0;
        this.aceite = false;
        this.listaEspera = false;
        this.realizado = false;
        this.altPrice = false;
        this.estadoClassif = 0;
    }
    
    /**
     * Construtor por parâmetro
     * @param mail           Mail do cliente que requisitou o aluguer.
     * @param matricula      Matrícula do veículo a alugar.
     * @param inicio         Data de início do aluguer.
     * @param fim            Data de fim do aluguer.
     * @param precoViagem    Preço pago pelo aluguer.
     * @param tempoAPe       Tempo a pé até ao veículo.
     * @param tempoViagem    Tempo total da viagem.
     * @param dest           Destino do Cliente.
     * @param dist           Distância em KM do cliente ao destino.
     * @param aceite         True se a viagem foi aceite, ou false se foi recusada.
     * @param listaEspera    True se se encontra em lista de espera ou false caso contrário.
     * @param done           True se o aluguer já foi realizado, ou false caso contrário.
     * @param altPrc         True se o preço foi alterado e false, caso contrário.
     * @param classif        0 - não foi efetuada nenhuma classificação,
     *                       1 - classificação dada apenas ao cliente,
     *                       2 - classificação dada apenas ao veículo,
     *                       3 - classificação foi dada a ambos
     */
    public Aluguer(String mail, String matricula, GregorianCalendar inicio, GregorianCalendar fim, double precoViagem,
                   int tempoAPe, int tempoViagem, Coordinate dest, double dist, boolean aceite, boolean listaEspera,
                   boolean done, boolean altPrc, int classif){
        this.mail = mail;
        this.matricula = matricula;
        this.dataInicio = inicio;
        this.dataFim = fim;
        this.custoViagem = precoViagem;
        this.tempoAteVeiculo = tempoAPe;
        this.tempoViagem = tempoViagem;
        this.destino = dest;
        this.distancia = dist;
        this.aceite = aceite;
        this.listaEspera = listaEspera;
        this.realizado = done;
        this.altPrice = altPrc;
        this.estadoClassif = classif;
    }
    
    /**
     * Construtor por cópia.
     * @param outroAluguer Um outro aluguer.
     */
    public Aluguer(Aluguer outroAluguer){
        this.mail = outroAluguer.getEmail();
        this.matricula = outroAluguer.getMatricula();
        this.dataInicio = outroAluguer.getDataInicio();
        this.dataFim = outroAluguer.getDataFim();
        this.custoViagem = outroAluguer.getCustoViagem();
        this.tempoAteVeiculo = outroAluguer.getTempoAteVeiculo();
        this.tempoViagem = outroAluguer.getTempoViagem();
        this.destino = outroAluguer.getDestino();
        this.distancia = outroAluguer.getDistancia();
        this.aceite = outroAluguer.getAceite();
        this.listaEspera = outroAluguer.getListaEspera();
        this.realizado = outroAluguer.getRealizado();
        this.altPrice = outroAluguer.getAlteraPreco();
        this.estadoClassif = outroAluguer.getEstadoClassificacao();
    }

    /*****************************************************************************
     *                             BEGIN GETTERS                                 *
     *****************************************************************************/
    
    /**
     * Devolve o mail do cliente.
     * @return String Representa o email do cliente.
     */
    public String getEmail(){return this.mail;}
    
    /**
     * Devolve a matrícula do veículo alugado pelo cliente.
     * @return String Representa a matrícula do veículo.
     */
    public String getMatricula(){return this.matricula;}
    
    /**
     * Devolve a data de início do aluguer.
     * @return GregorianCalendar Representa a data de início do aluguer.
     */
    public GregorianCalendar getDataInicio(){return this.dataInicio;}
        
    /**
     * Devolve a data de fim do aluguer.
     * @return GregorianCalendar Representa a data de fim do aluguer.
     */
    public GregorianCalendar getDataFim(){return this.dataFim;}
    
    /**
     * Devolve o custo total da viagem.
     * @return double Representa o custo total da viagem.
     */
    public double getCustoViagem(){return this.custoViagem;}
    
    /**
     * Devolve o tempo (em segundos) que o cliente demora a pé até ao veículo.
     * @return int Representa o tempo a pé até ao veículo (em segundos).
     */
    public int getTempoAteVeiculo(){return this.tempoAteVeiculo;}
    
    /**
     * Devolve o tempo (em segundos) total da viagem.
     * @return int Representa o tempo total da viagem.
     */
    public int getTempoViagem(){return this.tempoViagem;}

    /**
     * Devolve o destino do cliente.
     * @return Coordinate Representa as coordenadas de destino do cliente.
     */
    public Coordinate getDestino(){return this.destino;}

    /**
     * Devolve a distância (em Km) da origem (posição do cliente) até ao destino.
     * @return double Representa a distância (em Km) da origem (posição do cliente) até ao destino.
     */
    public double getDistancia(){return this.distancia;}
    
    /**
     * Devolve true ou false, caso o aluguer tenha sido aceite pelo proprietário do veículo ou não, respetivamente.
     * @return boolean Representa se a viagem foi aceite ou recusada pelo proprietário.
     */
    public boolean getAceite(){return this.aceite;}
    
    /**
     * Devolve true ou false, caso o aluguer esteja em lista de espera, ou não.
     * @return boolean Representa se o aluguer se encontra à espera de ser aceite ou não.
     */
    public boolean getListaEspera(){return this.listaEspera;}

    /**
     * Devolve true ou false, caso o aluguer tenha sido realizado, ou não, respetivamente.
     * @return boolean Representa se o aluguer foi realizado ou não.
     */
    public boolean getRealizado(){return this.realizado;}

    /**
     * Devolve true ou false, caso o preço do aluguer tenha sido alterado pelo proprietário.
     * @return boolean Representa se foi alterado o preço do aluguer ou não.
     */
    public boolean getAlteraPreco(){return this.altPrice;}

    /**
     * Devolve um inteiro que representa o estado atual da classificação do aluguer.
     * @return int Representa se o estado da classificação do aluguer,
     *             0 - não foi efetuada nenhuma classificação,
     *             1 - classificação dada apenas ao cliente,
     *             2 - classificação dada apenas ao veículo,
     *             3 - classificação foi dada a ambos.
     */
    public int getEstadoClassificacao(){return this.estadoClassif;}
    
    /*****************************************************************************
     *                              END GETTERS                                  *
     *****************************************************************************/
    
    /*****************************************************************************
     *                             BEGIN SETTERS                                 *
     *****************************************************************************/
    
    /**
     * Altera o custo total da viagem.
     */
    public void setCustoViagem(double newCost){this.custoViagem = newCost;}
    
    /**
     * Altera o valor do campo aceite.
     */
    public void setAceite(boolean aceite){this.aceite = aceite;}
    
    /**
     * Altera o valor do campo listaEspera.
     */
    public void setListaEspera(boolean listaEspera){this.listaEspera = listaEspera;}

    /**
     * Altera o valor do campo realizado.
     */
    public void setRealizado(boolean done){this.realizado = done;}
    
    /**
     * Altera o valor do campo listaEspera.
     */
    public void setAlteraPreco(boolean altPrc){this.altPrice = altPrc;}

    /**
     * Altera o valor do campo estado de classificação.
     */
    public void setEstadoClassificacao(int estado){this.estadoClassif = estado;}

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
        Aluguer alug = (Aluguer) obj;
        return  this.mail.equals(alug.getEmail()) && this.matricula.equals(alug.getMatricula()) &&
                this.dataInicio.equals(alug.getDataInicio()) && this.dataFim.equals(alug.getDataFim()) &&
                this.custoViagem == alug.getCustoViagem() && this.tempoAteVeiculo == alug.getTempoAteVeiculo() &&
                this.tempoViagem == alug.getTempoViagem() && this.aceite == alug.getAceite() &&
                this.listaEspera == alug.getListaEspera() && this.realizado == alug.getRealizado() &&
                this.altPrice == alug.getAlteraPreco() && this.estadoClassif == alug.getEstadoClassificacao();
    }
    
    /** 
     * Devolve uma representação do objeto em formato textual.
     * @return String Representa o objeto em formato textual.
     */
    public String toString() {
       StringBuilder str = new StringBuilder();
       
       str.append("*****    ALUGUER    *****\n"); 
       str.append("Mail do Cliente: "); str.append(this.mail); str.append("\n");
       str.append("Matrícula do Veículo: "); str.append(this.matricula); str.append("\n");
       str.append("Data de início do aluguer: "); str.append(this.dataInicio); str.append("\n");
       str.append("Data de fim do aluguer: "); str.append(this.dataFim); str.append("\n");
       str.append("Custo da Viagem: "); str.append(this.custoViagem); str.append("\n");
       str.append("Tempo a pé até ao veículo: "); str.append(this.tempoAteVeiculo); str.append("\n");
       str.append("Tempo total da viagem: "); str.append(this.tempoViagem); str.append("\n");
       str.append("Aluguer Aceite pelo Proprietário: "); str.append(this.aceite); str.append("\n");
       str.append("Aluguer em lista de espera: "); str.append(this.listaEspera); str.append("\n");
       str.append("Aluguer Realizado: "); str.append(this.realizado); str.append("\n");
       str.append("Preço modificado pelo proprietário do veículo: "); str.append(this.altPrice); str.append("\n");
       str.append("Estado da classificação: "); str.append(this.estadoClassif); str.append("\n");
        
       return str.toString();         
    }
    
    /**
     * Retorna uma cópia da instância.
     * @return Aluguer Um novo aluguer que é cópia do this.
     */
    public Aluguer clone(){
        return new Aluguer(this);
    }  
}
