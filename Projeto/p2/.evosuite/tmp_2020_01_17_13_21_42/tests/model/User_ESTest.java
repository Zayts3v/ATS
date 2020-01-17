/*
 * This file was automatically generated by EvoSuite
 * Fri Jan 17 14:08:00 GMT 2020
 */

package model;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import model.Car;
import model.Client;
import model.Owner;
import model.Rental;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;
import utils.Point;

@RunWith(EvoRunner.class) @EvoRunnerParameters(mockJVMNonDeterminism = true, useVFS = true, useVNET = true, resetStaticState = true, separateClassLoader = true, useJEE = true) 
public class User_ESTest extends User_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test00()  throws Throwable  {
      Double double0 = new Double((-3426.8));
      Point point0 = new Point(double0, double0);
      Client client0 = new Client(point0, "model.User", "3+@i", "model.User", "3+@i", 1385);
      Owner owner0 = new Owner("", "", "3+@i", 1385, "model.User");
      Car.CarType car_CarType0 = Car.CarType.ANY;
      Car car0 = new Car("", owner0, car_CarType0, 0.0, 2.0, 1.2, 1, point0, "&fK_cXS6I[2%4XO;M");
      Rental rental0 = new Rental(car0, client0, point0);
      client0.rate(rental0, 1385, 1385);
      client0.rate(rental0, 0, 0);
      int int0 = owner0.getRates();
      assertEquals(692, int0);
  }

  @Test(timeout = 4000)
  public void test01()  throws Throwable  {
      Double double0 = new Double(1.0);
      Point point0 = new Point(double0, double0);
      Client client0 = new Client(point0, "_d", (String) null, "_d", (String) null, (-623));
      assertEquals(100, client0.getRates());
      
      client0.rate(0);
      int int0 = client0.getRates();
      assertEquals(0, int0);
  }

  @Test(timeout = 4000)
  public void test02()  throws Throwable  {
      Double double0 = new Double(1315.93);
      Point point0 = new Point(double0, double0);
      Client client0 = new Client(point0, "[)$\"q#RH5hE#", "[)$\"q#RH5hE#", "[)$\"q#RH5hE#", "[)$\"q#RH5hE#", 1823);
      client0.rate((-605));
      int int0 = client0.getRates();
      assertEquals((-605), int0);
  }

  @Test(timeout = 4000)
  public void test03()  throws Throwable  {
      Owner owner0 = new Owner((String) null, (String) null, (String) null, 1, (String) null);
      owner0.getPasswd();
      assertEquals(100, owner0.getRates());
  }

  @Test(timeout = 4000)
  public void test04()  throws Throwable  {
      Owner owner0 = new Owner("@'D", "@'D", "~", 0, "=60.:MgbUtvNyN:=");
      String string0 = owner0.getPasswd();
      assertEquals(100, owner0.getRates());
      assertEquals("=60.:MgbUtvNyN:=", string0);
  }

  @Test(timeout = 4000)
  public void test05()  throws Throwable  {
      Double double0 = new Double(1.0);
      Point point0 = new Point(double0, double0);
      Client client0 = new Client(point0, "", "", "", "", 1922);
      client0.getPasswd();
      assertEquals(100, client0.getRates());
  }

  @Test(timeout = 4000)
  public void test06()  throws Throwable  {
      Double double0 = new Double((-819.29183));
      Point point0 = new Point(double0, double0);
      Client client0 = new Client(point0, (String) null, (String) null, (String) null, (String) null, 809);
      client0.getEmail();
      assertEquals(100, client0.getRates());
  }

  @Test(timeout = 4000)
  public void test07()  throws Throwable  {
      Owner owner0 = new Owner("IJEa", "Dono:           ", "Dono:           ", (-1), ">nAV'`<,");
      String string0 = owner0.getEmail();
      assertEquals(100, owner0.getRates());
      assertEquals("IJEa", string0);
  }

  @Test(timeout = 4000)
  public void test08()  throws Throwable  {
      Double double0 = new Double(2112.8);
      Point point0 = new Point(double0, double0);
      Client client0 = new Client(point0, "", "", "", "", 0);
      client0.getEmail();
      assertEquals(100, client0.getRates());
  }

  @Test(timeout = 4000)
  public void test09()  throws Throwable  {
      Double double0 = new Double((-2130.77587327758));
      Point point0 = new Point(double0, double0);
      Client client0 = new Client(point0, "", "", "", "", 0);
      client0.equals(client0);
      assertEquals(100, client0.getRates());
  }

  @Test(timeout = 4000)
  public void test10()  throws Throwable  {
      Owner owner0 = new Owner("v`3#&.Tv^r{\"J`#u`Q", "v`3#&.Tv^r{\"J`#u`Q", "", 0, "");
      Object object0 = new Object();
      owner0.equals(object0);
      assertEquals(100, owner0.getRates());
  }

  @Test(timeout = 4000)
  public void test11()  throws Throwable  {
      Client client0 = new Client((Point) null, "U#eX]xk]1xqobcc", "model.User", "model.User", "", (-8));
      Client client1 = new Client((Point) null, "U#eX]xk]1xqobcc", "U#eX]xk]1xqobcc", "r^}SIKmh3^KmTD&#l", "g6YGuc@4=\"G@;", (-1));
      // Undeclared exception!
      try { 
        client0.equals(client1);
        fail("Expecting exception: NullPointerException");
      
      } catch(NullPointerException e) {
         //
         // no message in exception (getMessage() returned null)
         //
         verifyException("model.Client", e);
      }
  }

  @Test(timeout = 4000)
  public void test12()  throws Throwable  {
      Double double0 = Double.valueOf((-41.20774459401005));
      Point point0 = new Point(double0, double0);
      Client client0 = new Client(point0, "^FM-5\b", "^FM-5\b", "^FM-5\b", "^FM-5\b", 2634);
      int int0 = client0.getRates();
      assertEquals(100, int0);
  }

  @Test(timeout = 4000)
  public void test13()  throws Throwable  {
      Double double0 = Double.valueOf((-41.20774459401005));
      Point point0 = new Point(double0, double0);
      Client client0 = new Client(point0, "^FM-5\b", "^FM-5\b", "^FM-5\b", "^FM-5\b", 2634);
      client0.hashCode();
      assertEquals(100, client0.getRates());
  }

  @Test(timeout = 4000)
  public void test14()  throws Throwable  {
      Double double0 = Double.valueOf((-41.20774459401005));
      Point point0 = new Point(double0, double0);
      Client client0 = new Client(point0, "^FM-5\b", "^FM-5\b", "^FM-5\b", "^FM-5\b", 2634);
      Client client1 = new Client(client0);
      assertEquals(100, client1.getRates());
  }
}
