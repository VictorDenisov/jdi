import java.util.concurrent.RunnableFuture;
import java.util.concurrent.TimeUnit;

public class Main implements RunnableFuture<String> {
    static public int f1 = 10;
    static private String fprivate = "fprivate_value";
    private String[] args;

    public Main(String[] args) {
        this.args = args;
    }

    public static void main(String[] args) {
        new Thread(new Main(args)).start();
    }

    public static String anotherMethod(long arg1, int arg2, String arg3, long arg4) {
        int internalVariable = 0;
        System.out.println("Hello afterInternal" + internalVariable);
        int internalVariable1 = 1;
        System.out.println("Hello afterInternal1" + internalVariable1);
        return "";
    }

    public void run() {
        System.out.println("Hello all");
        System.out.println("Arguments are");
        for (int i = 0; i < args.length; ++i) {
            System.out.println(i + ": " + args[i]);
        }
    }

    public boolean cancel(boolean v) {
        return true;
    }

    public boolean isCancelled() {
        return true;
    }

    public boolean isDone() {
        return true;
    }

    public String get() {
        return "";
    }

    public String get(long timeout, TimeUnit unit) {
        return "";
    }
}
