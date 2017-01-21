import java.util.Collections;
import java.util.ArrayList;
import java.util.List;
import static org.junit.Assert.*;
import org.junit.contrib.theories.*;
import org.junit.runner.RunWith;
import com.pholser.junit.quickcheck.ForAll;

@RunWith(Theories.class)
public class RevTestCase {
    @Theory public void revUnit(@ForAll Integer x) {
        ArrayList xs = new ArrayList();
        xs.add(x);
        assertEquals(rev(xs), xs);
    }

    @Theory public void revApp(
            @ForAll ArrayList<Integer> xs,
            @ForAll ArrayList<Integer> ys
    ) {
        assertEquals(
            rev(app(xs, ys)),
            app(rev(ys), rev(xs))
        );
    }

    static <A> List<A> rev(List<A> xs) {
        ArrayList<A> ys = new ArrayList<>();
        ys.addAll(xs);
        Collections.reverse(ys);
        return ys;
    }

    static <A> List<A> app(List<A> xs, List<A> ys) {
        ArrayList<A> zs = new ArrayList<>();
        zs.addAll(xs);
        zs.addAll(ys);
        return zs;
    }
}
