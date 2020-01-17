/*
 * This file was automatically generated by EvoSuite
 * Fri Jan 17 14:28:16 GMT 2020
 */

package view.viewmodel;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;
import utils.Point;
import view.viewmodel.CheapestNearCar;

@RunWith(EvoRunner.class) @EvoRunnerParameters(mockJVMNonDeterminism = true, useVFS = true, useVNET = true, resetStaticState = true, separateClassLoader = true, useJEE = true) 
public class CheapestNearCar_ESTest extends CheapestNearCar_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      Double double0 = Double.valueOf((-2831.39669));
      Point point0 = new Point(double0, double0);
      CheapestNearCar cheapestNearCar0 = null;
      try {
        cheapestNearCar0 = new CheapestNearCar(point0, 0, (String) null);
        fail("Expecting exception: NullPointerException");
      
      } catch(NullPointerException e) {
         //
         // no message in exception (getMessage() returned null)
         //
         verifyException("view.viewmodel.CheapestNearCar", e);
      }
  }

  @Test(timeout = 4000)
  public void test1()  throws Throwable  {
      CheapestNearCar cheapestNearCar0 = null;
      try {
        cheapestNearCar0 = new CheapestNearCar((Point) null, (-6409), "view.viewmodel.CheapestNearCar");
        fail("Expecting exception: Exception");
      
      } catch(Throwable e) {
         //
         // no message in exception (getMessage() returned null)
         //
         verifyException("view.viewmodel.CheapestNearCar", e);
      }
  }
}
