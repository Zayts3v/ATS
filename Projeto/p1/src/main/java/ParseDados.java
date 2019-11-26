/**
 * Classe que contém métodos para efetuar parsing de dados.
 *
 * 
 * 
 * 
 * @version 20190415
 */

import java.util.GregorianCalendar;
import java.util.Random;
import java.util.ArrayList;
import java.util.InputMismatchException;

import static java.lang.System.out;

public class ParseDados{

    public static Proprietario parseProprietario(String linha){
        String nome, nif, email, morada;
        String [] dados = linha.split(",");
        int ano = new Random().ints(1950, 2000).findFirst().getAsInt();
        int mes = new Random().ints(0, 11).findFirst().getAsInt();
        int dia = new Random().ints(0, 30).findFirst().getAsInt();
        GregorianCalendar date = new GregorianCalendar(ano, mes, dia);

        nome = dados[0];
        nif = dados[1];
        email = dados[2];
        morada = dados[3];

        Proprietario prop = new Proprietario(nome, nif, email, nif, morada, date);

        return prop.clone();
    }

    public static Cliente parseCliente(String linha){
        String nome, nif, email, morada;
        String [] dados = linha.split(",");
        int ano = new Random().ints(1950, 2000).findFirst().getAsInt();
        int mes = new Random().ints(1, 12).findFirst().getAsInt();
        int dia = new Random().ints(1, 31).findFirst().getAsInt();
        GregorianCalendar date = new GregorianCalendar(ano, mes, dia);
        double x = 0.0, y = 0.0;


        nome = dados[0];
        nif = dados[1];
        email = dados[2];
        morada = dados[3];

        try {
            x = Double.parseDouble(dados[4]);
            y = Double.parseDouble(dados[5]);
        }
        catch(InputMismatchException exc){
        }

        Coordinate cords = new Coordinate(x,y);

        Cliente cli = new Cliente(nome, nif, email, nif, morada, date, cords, 0, 0, 0.0);

        return cli.clone();

    }

    public static Veiculo parseVeiculo(String linha){
        String [] dados = linha.split(",");

        switch(dados[0]){
            case "Electrico":
                Veiculo ce = parseCarroEletrico(linha);
                return ce.clone();
            case "Hibrido":
                Veiculo ch = parseCarroHibrido(linha);
                return ch.clone();
            case "Gasolina":
                Veiculo cg = parseCarroGasolina(linha);
                return cg.clone();
        }
        return new Veiculo();
    }

    private static CarroEletrico parseCarroEletrico(String linha){
        String marca, matricula, nif, email, morada;
        String [] dados = linha.split(",");
        int velocidade, autonomia;
        double x, y, preco, consumo;

        marca = dados[1];
        matricula = dados[2];
        nif = dados[3];

        try {
            velocidade = Integer.parseInt(dados[4]);
            preco = Double.parseDouble(dados[5]);
            consumo = Double.parseDouble(dados[6]);
            autonomia = Integer.parseInt(dados[7]);
            x = Double.parseDouble(dados[8]);
            y = Double.parseDouble(dados[9]);
        }
        catch(InputMismatchException exc){return new CarroEletrico();}

        Coordinate cords = new Coordinate(x,y);

        CarroEletrico car = new CarroEletrico(marca, matricula, nif, velocidade, preco, consumo,
                autonomia, cords, true, 0, new ArrayList<ParDatas>());
        return car;
    }

    private static CarroGasolina parseCarroGasolina(String linha){
        String marca, matricula, nif, email, morada;
        String [] dados = linha.split(",");
        int velocidade, autonomia;
        double x, y, preco, consumo;

        marca = dados[1];
        matricula = dados[2];
        nif = dados[3];

        try {
            velocidade = Integer.parseInt(dados[4]);
            preco = Double.parseDouble(dados[5]);
            consumo = Double.parseDouble(dados[6]);
            autonomia = Integer.parseInt(dados[7]);
            x = Double.parseDouble(dados[8]);
            y = Double.parseDouble(dados[9]);
        }
        catch(InputMismatchException exc){return new CarroGasolina();}

        Coordinate cords = new Coordinate(x,y);

        CarroGasolina car = new CarroGasolina(marca, matricula, nif, velocidade, preco, consumo,
                autonomia, cords, true, 0, new ArrayList<ParDatas>());
        return car;
    }

    private static CarroHibrido parseCarroHibrido(String linha){
        String marca, matricula, nif, email, morada;
        String [] dados = linha.split(",");
        int velocidade, autonomia;
        double x, y, preco, consumo;

        marca = dados[1];
        matricula = dados[2];
        nif = dados[3];

        try {
            velocidade = Integer.parseInt(dados[4]);
            preco = Double.parseDouble(dados[5]);
            consumo = Double.parseDouble(dados[6]);
            autonomia = Integer.parseInt(dados[7]);
            x = Double.parseDouble(dados[8]);
            y = Double.parseDouble(dados[9]);
        }
        catch(InputMismatchException exc){return null;}

        Coordinate cords = new Coordinate(x,y);

        CarroHibrido car = new CarroHibrido(marca, matricula, nif, velocidade, preco, consumo,
                autonomia, cords, true, 0, new ArrayList<ParDatas>());
        return car;
    }
}
