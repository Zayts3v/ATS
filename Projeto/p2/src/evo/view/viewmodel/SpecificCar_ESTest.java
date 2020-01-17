/*
 * This file was automatically generated by EvoSuite
 * Fri Jan 17 14:30:56 GMT 2020
 */

package view.viewmodel;

import org.junit.Test;
import static org.junit.Assert.*;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;
import utils.Point;
import view.viewmodel.SpecificCar;

@RunWith(EvoRunner.class) @EvoRunnerParameters(mockJVMNonDeterminism = true, useVFS = true, useVNET = true, resetStaticState = true, separateClassLoader = true, useJEE = true) 
public class SpecificCar_ESTest extends SpecificCar_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      SpecificCar specificCar0 = new SpecificCar((Point) null, (String) null);
      Point point0 = specificCar0.getPoint();
      assertNull(point0);
  }

  @Test(timeout = 4000)
  public void test1()  throws Throwable  {
      SpecificCar specificCar0 = new SpecificCar((Point) null, (String) null);
      String string0 = specificCar0.getNumberPlate();
      assertNull(string0);
  }

  @Test(timeout = 4000)
  public void test2()  throws Throwable  {
      Double double0 = new Double(2.0);
      Point point0 = new Point(double0, double0);
      SpecificCar specificCar0 = new SpecificCar(point0, "");
      String string0 = specificCar0.getNumberPlate();
      assertEquals("", string0);
  }

  @Test(timeout = 4000)
  public void test3()  throws Throwable  {
      Double double0 = new Double(1030.91752306009);
      Point point0 = new Point(double0, double0);
      SpecificCar specificCar0 = new SpecificCar(point0, "tJ,");
      Point point1 = specificCar0.getPoint();
      assertSame(point0, point1);
  }

  @Test(timeout = 4000)
  public void test4()  throws Throwable  {
      Double double0 = new Double(1030.91752306009);
      Point point0 = new Point(double0, double0);
      SpecificCar specificCar0 = new SpecificCar(point0, "tJ,");
      String string0 = specificCar0.getNumberPlate();
      assertEquals("tJ,", string0);
  }
}
