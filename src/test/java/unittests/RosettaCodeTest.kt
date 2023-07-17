package unittests

import core.Repl
import core.environment.DefaultEnvironment
import core.procedures.io.Display
import core.procedures.io.Newline
import core.scm.OutputPort
import core.scm.Symbol
import org.junit.Assert.assertEquals
import org.junit.Test
import java.io.ByteArrayOutputStream
import java.math.BigInteger
import java.util.*

class RosettaCodeTest : AbstractTest() {

    companion object {
        private val LS = System.lineSeparator()
    }

    @Test
    fun testEvalGroupNumbers() {
        val group = "(let loop ((numbers '(3 -2 1 6 -5))" +
                    "           (nonneg '())" +
                    "           (neg '()))" +
                    "  (cond ((empty? numbers) (list nonneg neg))" +
                    "        ((>= (car numbers) 0)" +
                    "         (loop (cdr numbers)" +
                    "               (cons (car numbers) nonneg)" +
                    "               neg))" +
                    "        ((< (car numbers) 0)" +
                    "         (loop (cdr numbers)" +
                    "               nonneg" +
                    "               (cons (car numbers) neg)))))"

        assertEquals(listOf(listOf(6L, 1L, 3L), listOf(-5L, -2L)), eval(group))
    }

    @Test
    fun testFlattenList() {

        val flatten = "(define (flatten x)" +
                      "    (cond ((empty? x) '())" +
                      "          ((not (pair? x)) (list x))" +
                      "          (else (append (flatten (car x))" +
                      "                        (flatten (cdr x))))))"
        eval(flatten)
        assertEquals(listOf(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L), eval("(flatten '((1) 2 ((3 4) 5) ((())) (((6))) 7 8 ()))"))
    }

    @Test
    fun testQuickSort() {

        val splitby = "(define (split-by l p k)" +
                      "  (let loop ((low '())" +
                      "             (high '())" +
                      "             (l l))" +
                      "    (cond ((empty? l)" +
                      "           (k low high))" +
                      "          ((p (car l))" +
                      "           (loop low (cons (car l) high) (cdr l)))" +
                      "          (else" +
                      "           (loop (cons (car l) low) high (cdr l))))))"

        val quick = "(define (quicksort l gt?)" +
                    "  (if (empty? l)" +
                    "      '()" +
                    "      (split-by (cdr l) " +
                    "                (lambda (x) (gt? x (car l)))" +
                    "                (lambda (low high)" +
                    "                  (append (quicksort low gt?)" +
                    "                          (list (car l))" +
                    "                          (quicksort high gt?))))))"

        eval(splitby)
        eval(quick)
        assertEquals(listOf(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L), eval("(quicksort '(1 3 5 7 9 8 6 4 2) >)"))
    }

    @Test
    fun testGnomeSort() {

        val gnome = "(define (gnome-sort-compar in-order input-list)" +
                    "  (let gnome ((p (list (car input-list)))" +
                    "              (n (cdr input-list)))" +
                    "    (if (empty? n)" +
                    "        p" +
                    "        (let ((prev-pot (car p))" +
                    "              (next-pot (car n)))" +
                    "          (if (in-order next-pot prev-pot)" +
                    "              (gnome (cons next-pot p) (cdr n))" +
                    "              (if (empty? (cdr p))" +
                    "                  (gnome (list next-pot) (cons prev-pot (cdr n)))" +
                    "                  (gnome (cdr p) (cons next-pot (cons prev-pot (cdr n))))))))))"
        eval(gnome)

        val test = "(gnome-sort-compar <= '(98 36 2 78 5 81 32 90 73 21 94 28 53 25 10 99))"
        val sorted = listOf(2L, 5L, 10L, 21L, 25L, 28L, 32L, 36L, 53L, 73L, 78L, 81L, 90L, 94L, 98L, 99L)
        assertEquals(sorted, eval(test))
    }

    @Test
    fun testHailstoneSeq() {

        val hailstone = "(define (hailstone n)" +
                        "  (if (= n 1) '(1)" +
                        "  (cons n (hailstone (if (even? n) (/ n 2) (+ 1 (* 3 n)))))))"

        val hailstoneLength = "(define (hailstone-length n)" +
                              "  (let aux ((n n) (r 1)) (if (= n 1) r" +
                              "  (aux (if (even? n) (/ n 2) (+ 1 (* 3 n))) (+ r 1)))))"

        val hailstoneMax = "(define (hailstone-max a b)" +
                           "  (let aux ((i a) (j 0) (k 0))" +
                           "    (if (> i b) (list j k)" +
                           "      (let ((h (hailstone-length i)))" +
                           "        (if (> h k) (aux (+ i 1) i h) (aux (+ i 1) j k))))))"

        eval(hailstone)
        eval(hailstoneLength)
        eval(hailstoneMax)

        val seq = listOf(27L, 82L, 41L, 124L, 62L, 31L, 94L, 47L, 142L, 71L, 214L, 107L, 322L, 161L, 484L, 242L, 121L,
                         364L, 182L, 91L, 274L, 137L, 412L, 206L, 103L, 310L, 155L, 466L, 233L, 700L, 350L, 175L, 526L,
                         263L, 790L, 395L, 1186L, 593L, 1780L, 890L, 445L, 1336L, 668L, 334L, 167L, 502L, 251L, 754L,
                         377L, 1132L, 566L, 283L, 850L, 425L, 1276L, 638L, 319L, 958L, 479L, 1438L, 719L, 2158L, 1079L,
                         3238L, 1619L, 4858L, 2429L, 7288L, 3644L, 1822L, 911L, 2734L, 1367L, 4102L, 2051L, 6154L, 3077L,
                         9232L, 4616L, 2308L, 1154L, 577L, 1732L, 866L, 433L, 1300L, 650L, 325L, 976L, 488L, 244L, 122L,
                         61L, 184L, 92L, 46L, 23L, 70L, 35L, 106L, 53L, 160L, 80L, 40L, 20L, 10L, 5L, 16L, 8L, 4L, 2L, 1L)

        val res = eval("(hailstone 27)") as List<Number>
        for (i in seq.indices) {
            assertEquals(seq[i], res[i].toLong())
        }
        assertEquals(112L, eval("(hailstone-length 27)"))
    }

    @Test
    fun testEvalHornersRule() {
        val horner = "(define (horner lst x)" +
                   "  (define (*horner lst x acc)" +
                   "    (if (empty? lst)" +
                   "        acc" +
                   "        (*horner (cdr lst) x (+ (* acc x) (car lst)))))" +
                   "  (*horner (reverse lst) x 0))"
        eval(horner)
        assertEquals(128L, eval("(horner '(-19 7 -4 6) 3)"))
    }

    @Test
    fun testDayOfWeek() {

        val dayOfWeek = "(define (day-of-week year month day)" +
                        "  (if (< month 3)" +
                        "    (begin" +
                        "      (set! month (+ month 12))" +
                        "      (set! year (- year 1)))" +
                        "  #f)" +
                        "(+ 1 " +
                        "   (remainder (+ 5 day (quotient (* (+ 1 month) 13) 5) " +
                        "                 year (quotient year 4) (* (quotient year 100) 6) (quotient year 400)) " +
                        "              7)))"
        eval(dayOfWeek)
        assertEquals(3L, eval("(day-of-week 2016 6 29)"))

        // test Today
        val calendar = Calendar.getInstance()
        var dw: Int? = calendar.get(Calendar.DAY_OF_WEEK) - 1
        if (dw == 0) {
            dw = 7
        }
        val y = calendar.get(Calendar.YEAR)
        val m = calendar.get(Calendar.MONTH) + 1
        val d = calendar.get(Calendar.DAY_OF_MONTH)
        assertEquals(dw!!.toLong(), eval("(day-of-week $y $m $d)"))
    }

    @Test
    fun testEvalHanoi() {

        val old = Repl.currentOutputPort
        val baos = ByteArrayOutputStream()
        Repl.currentOutputPort = OutputPort(baos)

        val tempEnv = DefaultEnvironment()
        /* Eval lib procedures */
        tempEnv.libraryProcedures.forEach { eval(it) }
        tempEnv.put(Symbol.intern("display"), Display())
        tempEnv.put(Symbol.intern("newline"), Newline())

        val hanoi = "(define (hanoi n a b c) " +
                    "  (if (> n 0)" +
                    "    (begin" +
                    "      (hanoi (- n 1) a c b)" +
                    "      (display \"Move disk from pole \")" +
                    "      (display a)" +
                    "      (display \" to pole \")" +
                    "      (display b)" +
                    "      (newline)" +
                    "      (hanoi (- n 1) c b a))" +
                    " #t))"
        eval(hanoi)
        eval("(hanoi 4 1 2 3)")

        val solution = "Move disk from pole 1 to pole 3" + LS +
                       "Move disk from pole 1 to pole 2" + LS +
                       "Move disk from pole 3 to pole 2" + LS +
                       "Move disk from pole 1 to pole 3" + LS +
                       "Move disk from pole 2 to pole 1" + LS +
                       "Move disk from pole 2 to pole 3" + LS +
                       "Move disk from pole 1 to pole 3" + LS +
                       "Move disk from pole 1 to pole 2" + LS +
                       "Move disk from pole 3 to pole 2" + LS +
                       "Move disk from pole 3 to pole 1" + LS +
                       "Move disk from pole 2 to pole 1" + LS +
                       "Move disk from pole 3 to pole 2" + LS +
                       "Move disk from pole 1 to pole 3" + LS +
                       "Move disk from pole 1 to pole 2" + LS +
                       "Move disk from pole 3 to pole 2"

        assertEquals(solution, baos.toString().trim { it <= ' ' })
        Repl.currentOutputPort = old
    }

    @Test
    fun testIntegerRoots() {

        eval("(define // quotient)")
        eval("(define (root a b)" +
             "  (if (< b 2)" +
             "      b" +
             "    (let* ((a1 (- a 1))" +
             "           (c 1)" +
             "           (d (// (+ (* a1 c) (// b (expt c a1))) a))" +
             "           (e (// (+ (* a1 d) (// b (expt d a1))) a)))" +
             "      (y a a1 b c d e))))")

        eval("(define (y a a1 b c d e)" +
             "  (if (or (= c d) (= c e))" +
             "      (min d e)" +
             "    (y a a1 b d e (// (+ (* a1 e)" +
             "                         (// b (expt e a1))) a))))")

        assertEquals(2L, eval("(root 3 25)"))
        assertEquals(125L, eval("(root 3 (* 2 (expt 1000 2)))"))
        assertEquals(BigInteger.valueOf(10000000L), eval("(root 3 (expt 1000 7))"))
        assertEquals(BigInteger("125992104989487316476"), eval("(root 3 (* 2 (expt 1000 20)))"))
        assertEquals(BigInteger("125992104989487316476721060727822835057025146470150798008197511215529967651395948372939656243625509415431025603561566525939902404061373722845911030426935524696064261662500097747452656548030686718540551"),
                eval("(root 3 (* 2 (expt 1000 200)))"))

//      assertEquals(BigInteger("12599210498948731647672106072782283505702514647015079800819751121552996765139594837293965624362550941543102560356156652593990240406137372284591103042693552469606426166250009774745265654803068671854055186892458725167641993737096950983827831613991551293136953661839474634485765703031190958959847411059811629070535908164780114735213254847712978802422085820532579725266622026690056656081994715628176405060664826773572670419486207621442965694205079319172441480920448232840127470321964282081201905714188996459998317503801888689594202055922021154729973848802607363697417887792157984675099539630078260959624203483238660139857363433909737126527995991969968377913168168154428850279651529278107679714002040605674803938561251718357006907984996341976291474044834540269715476228513178020643878047649322579052898467085805286258130005429388560720609747223040631357234936458406575916916916727060124402896700001069081035313852902700415084232336239889386496782194149838027072957176812879001445746227147702348357151905506"),
//                   eval("(root 3 (* 2 (expt 1000 1000)))", tempEnv))
//      assertEquals(BigInteger("125992104989487316476721060727822835057025146470150798008197511215529967651395948372939656243625509415431025603561566525939902404061373722845911030426935524696064261662500097747452656548030686718540551868924587251676419937370969509838278316139915512931369536618394746344857657030311909589598474110598116290705359081647801147352132548477129788024220858205325797252666220266900566560819947156281764050606648267735726704194862076214429656942050793191724414809204482328401274703219642820812019057141889964599983175038018886895942020559220211547299738488026073636974178877921579846750995396300782609596242034832386601398573634339097371265279959919699683779131681681544288502796515292781076797140020406056748039385612517183570069079849963419762914740448345402697154762285131780206438780476493225790528984670858052862581300054293885607206097472230406313572349364584065759169169167270601244028967000010690810353138529027004150842323362398893864967821941498380270729571768128790014457462271477023483571519055067220848184850092872392092826466067171742477537097370300127429180940544256965920750363575703751896037074739934610144901451576359604711119738452991329657262589048609788561801386773836157730098659836608059757560127871214868562426845564116515581793532280158962912994450040120842541416015752584162988142309735821530604057724253836453253356595511725228557956227724036656284687590154306675351908548451181817520429124123378096317252135754114181146612736604578303605744026513096070968164006888185657231009008428452608641405950336900307918699355691335183428569382625543135589735445023330285314932245513412195545782119650083395771426685063328419619686512109255789558850899686190154670043896878665545309854505763765036008943306510356935777537249548436821370317162162183495809356208726009626785183418345652239744540004476021778894208183802786665306532663261864116007400747475473558527701689502063754132232329694243701742343491617690600723853902227681129777413872079823430391031628546452083111122546828353183047061"),
//                   eval("(root 3 (* 2 (expt 1000 2000)))", tempEnv))
    }

    @Test
    fun testIntegerRootsIteration() {
        val code = "(let* ((// quotient)" +
                   "       (a  3)" +
                   "       (a1 2)" +
                   "       (b 100000000000000000000000000000000000000000000000000000000000000000)" +
                   "       (d 3549465341104592045951592)" +
                   "       (e (// (+ (* a1 d) (// b (expt d a1))) a)))" +
                   "   e)"

        assertEquals(BigInteger("2366310230048836102686780"), eval(code))
    }

    @Test
    fun testEvalQuine() {

        val old = Repl.currentOutputPort
        val baos = ByteArrayOutputStream()
        Repl.currentOutputPort = OutputPort(baos)

        val tempEnv = DefaultEnvironment()
        /* Eval lib procedures */
        tempEnv.libraryProcedures.forEach { eval(it) }
        tempEnv.put(Symbol.intern("display"), Display())

        val quine = "((lambda (s) (display (list s (list (quote quote) s))))" +
                    " (quote (lambda (s) (display (list s (list (quote quote) s))))))"
        eval(quine)
        assertEquals(quine, baos.toString().trim { it <= ' ' })

        Repl.currentOutputPort = old
    }

    @Test
    fun testManOrBoyKnuthsTest() {
        val A = "(define (A k x1 x2 x3 x4 x5)" +
                "  (define (B)" +
                "    (set! k (- k 1))" +
                "    (A k B x1 x2 x3 x4))" +
                "  (if (<= k 0)" +
                "      (+ (x4) (x5))" +
                "    (B)))"
        eval(A)

        val code = "(A 10 (lambda () 1) (lambda () -1) (lambda () -1) (lambda () 1) (lambda () 0))"
        assertEquals(-67L, eval(code))
    }

    @Test
    fun testHaversineFormula() {
        eval("(define earth-radius 6371)")
        eval("(define (distance lat1 long1 lat2 long2)" +
             "  (define (h a b) (expt (sin (/ (- b a) 2)) 2))" +
             "  (* 2 earth-radius " +
             "     (asin (sqrt (+ (h lat1 lat2) " +
             "                    (* (cos lat1) (cos lat2) (h long1 long2)))))))")
        eval("(define (deg-to-rad d m s) " + "  (* (/ pi 180) (+ d (/ m 60) (/ s 3600))))")
        assertEquals(2886.444442837984,
                eval("(distance (deg-to-rad 36  7.2 0) (deg-to-rad  86 40.2 0)" +
                     "          (deg-to-rad 33 56.4 0) (deg-to-rad 118 24.0 0))"))
    }
}
