public class Test {

    private int a;
    private int b;
    private int c;


    public Test(int a, int b) {
        this.a = a;
        this.b = b;
    }


    @Override
    public String toString() {
        return "Test{" +
                "a=" + a +
                ", b=" + b +
                ", c=" + c +
                '}';
    }

    public int getA() {
        return a;
    }

    public int getB() {
        return b;
    }

    public int getC() {
        return c;
    }

    public void setA(int a) {
        this.a = a;
    }

    public void setB(int b) {
        this.b = b;
    }

    public void setC(int c) {
        this.c = c;
    }
}
