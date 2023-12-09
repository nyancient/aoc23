import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.Arrays;
import java.util.stream.Stream;

class Dec09 {
    public static int[] readNumberSeries(String ln) {
        return Arrays.stream(ln.split(" ")).mapToInt(Integer::parseInt).toArray();
    }

    public static Stream<int[]> readInput(BufferedReader reader) {
        return reader.lines().map(Dec09::readNumberSeries);
    }

    public static int getNextNumber(int[] xs, boolean backwards) {
        int[] changes = new int[xs.length - 1];
        for(int i = 1; i < xs.length; i++) {
            changes[i - 1] = xs[i] - xs[i-1];
        }
        if(Arrays.stream(changes).allMatch(x -> x == 0)) {
            return xs[0];
        } else {
            if(backwards) {
                return xs[0] - getNextNumber(changes, backwards);
            } else {
                return xs[xs.length - 1] + getNextNumber(changes, backwards);
            }
        }
    }

    public static void main(String[] args) {
        BufferedReader r = new BufferedReader(new InputStreamReader(System.in));
        final boolean backwards = args.length >= 1 && args[0].equals("--backwards") ? true : false;
        System.out.println(readInput(r).mapToInt(x -> Dec09.getNextNumber(x, backwards)).sum());
    }
        
}
