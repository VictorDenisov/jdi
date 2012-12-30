public class Main {
    public int f1;
    private String fprivate;

    public static void main(String[] args) {
        System.out.println("Hello all");
        System.out.println("Arguments are");
        for (int i = 0; i < args.length; ++i) {
            System.out.println(i + ": " + args[i]);
        }
    }

    public static String anotherMethod(long arg1, int arg2, String arg3, long arg4) {
        int internalVariable = 0;
        System.out.println("Hello afterInternal" + internalVariable);
        int internalVariable1 = 1;
        System.out.println("Hello afterInternal1" + internalVariable1);
        return "";
    }
}
