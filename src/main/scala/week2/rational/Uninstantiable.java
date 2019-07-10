package week2.rational;

public class Uninstantiable {

    public static int badField = willThrow();

    public static int willThrow()  {
        throw new RuntimeException("willThrow()");
    }

}
