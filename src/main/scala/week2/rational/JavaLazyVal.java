package week2.rational;

public class JavaLazyVal {

    private long val;

    // Pay once.
    public long getVal() {
        if (val == 1) {
            System.out.println("Performing an expensive computation...");
            val = (long) Math.pow(3, 10);   // Assumed expensive. Have to ensure that value CAN'T be equal to the initializer value!
        } else {
            System.out.println("No longer performing expensive computation...");
        }
        return val;
    }

    public JavaLazyVal() {
        val = 1;
    }


}
