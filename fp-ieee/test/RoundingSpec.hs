{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE HexFloatLiterals #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}
module RoundingSpec where
import           Control.Monad
import           Data.Proxy
import           Data.Ratio
import           Numeric
import           Numeric.Floating.IEEE
import           Numeric.Floating.IEEE.Internal
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck hiding (classify)
import           Util

newtype RoundToOdd a = RoundToOdd { roundToOdd :: a }
  deriving (Functor)

instance RoundingStrategy RoundToOdd where
  exact = RoundToOdd
  inexact _o _neg parity zero away | even parity = RoundToOdd away
                                   | otherwise = RoundToOdd zero
  doRound exact _o _neg parity zero away | not exact && even parity = RoundToOdd away
                                         | otherwise = RoundToOdd zero

newtype Exactness a = Exactness { isExact :: Bool }
  deriving (Functor)

instance RoundingStrategy Exactness where
  exact _ = Exactness True
  inexact _o _neg _parity _zero _away = Exactness False
  doRound exact _o _neg _parity _zero _away = Exactness exact

prop_fromIntegerR_vs_fromRationalR :: (RealFloat a, RoundingStrategy f) => Proxy a -> (f a -> a) -> Integer -> Property
prop_fromIntegerR_vs_fromRationalR _ f m =
  let x = f (fromIntegerR m)
      y = f (fromRationalR (m % 1))
  in x `sameFloatP` y

prop_fromIntegerR_vs_encodeFloatR :: (RealFloat a, RoundingStrategy f) => Proxy a -> (f a -> a) -> Integer -> NonNegative Int -> Property
prop_fromIntegerR_vs_encodeFloatR _ f m (NonNegative k) =
  let x = f (fromIntegerR m)
      y = f (encodeFloatR (m * floatRadix x ^ k) (-k))
  in x `sameFloatP` y

prop_fromRationalR_vs_encodeFloatR :: (RealFloat a, RoundingStrategy f) => Proxy a -> (f a -> a) -> Integer -> Int -> Property
prop_fromRationalR_vs_encodeFloatR _ f m k =
  let x = f (fromRationalR (fromInteger m * fromInteger (floatRadix x) ^^ k))
      y = f (encodeFloatR m k)
  in x `sameFloatP` y

prop_fromRationalR_vs_fromRational :: RealFloat a => Proxy a -> Rational -> Property
prop_fromRationalR_vs_fromRational proxy q =
  let x = roundTiesToEven (fromRationalR q) `asProxyTypeOf` proxy
      y = fromRational q `asProxyTypeOf` proxy
  in x `sameFloatP` y

prop_order :: RealFloat a => Proxy a -> (forall f. RoundingStrategy f => f a) -> Property
prop_order _ result =
  let tiesToEven = roundTiesToEven result
      tiesToAway = roundTiesToAway result
      tiesTowardZero = roundTiesTowardZero result
      up = roundTowardPositive result
      down = roundTowardNegative result
      zero = roundTowardZero result
      toOdd = roundToOdd result
  in if isExact result then
       counterexample "exact case" $ conjoin
       [ counterexample "tiesToAway == tiesToEven" $ tiesToAway `sameFloatP` tiesToEven
       , counterexample "tiesTowardZero == tiesToEven" $ tiesTowardZero `sameFloatP` tiesToEven
       , counterexample "upward == tiesToEven" $ up `sameFloatP` tiesToEven
       , counterexample "downward == tiesToEven" $ down `sameFloatP` tiesToEven
       , counterexample "towardZero == tiesToEven" $ zero `sameFloatP` tiesToEven
       , counterexample "toOdd == tiesToEven" $ toOdd `sameFloatP` tiesToEven
       ]
     else
       counterexample "inexact case" $ conjoin
       [ counterexample "down < up" $ down < up
       , counterexample "down <= tiesToEven" $ down <= tiesToEven
       , counterexample "down <= tiesToAway" $ down <= tiesToAway
       , counterexample "down <= tiesTowardZero" $ down <= tiesTowardZero
       , counterexample "down <= towardZero" $ down <= zero
       , counterexample "down <= odd" $ down <= toOdd
       , counterexample "tiesToEven <= up" $ tiesToEven <= up
       , counterexample "tiesToAway <= up" $ tiesToAway <= up
       , counterexample "tiesTowardZero <= up" $ tiesTowardZero <= up
       , counterexample "towardZero <= up" $ zero <= up
       , counterexample "odd <= up" $ toOdd <= up
       , counterexample "nextUp down == up" $ nextUp down `sameFloatP` up
       , counterexample "down == nextDown up" $ down `sameFloatP` nextDown up
       , counterexample "abs towardZero < max (abs down) (abs up)" $ abs zero < max (abs down) (abs up)
       , counterexample "not (isMantissaEven toOdd)" $ not (isMantissaEven toOdd)
       ]

prop_add_roundToOdd :: RealFloat a => Proxy a -> a -> a -> Property
prop_add_roundToOdd _ x y = isFinite x && isFinite y && isFinite (x + y) ==>
  let z = add_roundToOdd x y
      w = if x == 0 && y == 0 then
            x + y
          else
            roundToOdd (fromRationalR (toRational x + toRational y))
  in z `sameFloatP` w

eachStrategy :: Testable prop => (forall f. RoundingStrategy f => (f a -> a) -> prop) -> Property
eachStrategy p = conjoin
  [ counterexample "roundTiesToEven" (p roundTiesToEven)
  , counterexample "roundTiesToAway" (p roundTiesToAway)
  , counterexample "roundTiesTowardZero" (p roundTiesTowardZero)
  , counterexample "roundTowardPositive" (p roundTowardPositive)
  , counterexample "roundTowardNegative" (p roundTowardNegative)
  , counterexample "roundTowardZero" (p roundTowardZero)
  , counterexample "roundToOdd" (p roundToOdd)
  ]

testUnary :: RealFloat b => (a -> b) -> [(String, a, b)] -> Property
testUnary f cases = conjoin
  [ counterexample t $ f a `sameFloatP` result
  | (t,a,result) <- cases
  ]

{-# NOINLINE spec #-}
spec :: Spec
spec = do
  describe "Double" $ do
    let proxy :: Proxy Double
        proxy = Proxy
    prop "fromIntegerR vs fromRationalR" $ eachStrategy (prop_fromIntegerR_vs_fromRationalR proxy)
    prop "fromIntegerR vs encodeFloatR" $ eachStrategy (prop_fromIntegerR_vs_encodeFloatR proxy)
    prop "fromRationalR vs encodeFloatR" $ eachStrategy (prop_fromRationalR_vs_encodeFloatR proxy)
    prop "fromRationalR vs fromRational" $ prop_fromRationalR_vs_fromRational proxy
    prop "result of fromIntegerR" $ \x -> prop_order proxy (fromIntegerR x)
    prop "result of fromRationalR" $ \x -> prop_order proxy (fromRationalR x)
    prop "result of encodeFloatR" $ \m k -> prop_order proxy (encodeFloatR m k)
    prop "add_roundToOdd" $ forAllFloats2 $ prop_add_roundToOdd proxy

    do let cases :: [(String, Rational, Double)]
           cases = [ let t = 11435996997111233 % 1660860084017817297360368008619370227400073727045418226348482155039064904553019973177107435363660614816513137110180404061646380785658477636245443559462428597275694780106044074992747404797486457853074429979899122551795724461450521406238742712434733270295344316890429535153317233021396948961884411359194146958100478088711873454042107514097515809485603670823814576138204139943337375836756405167181947093525325738801370702465460537395969617160395178613194019276299200610817420725783045671692771793360418111369105879747924354309959938911042057102540038489527102833880604228417018090258140649799612644290906038462100262234760641844967425501906703079079531111883261520094019262965803907605528809355522427428605283171700681998722400652411744851193007546978988038363226440325125816593274436339451950472293881264365176866099134907912252035904613356400091473040550399623768278773198402959131216609632370028659088546103031543716668650443675061896807069455112892464207615075528889823150217287305246018046657536654015550308954692439217754082060020956581265580805928178408368880094563736441111304424147055967579092700683418565515720301167266647150173895623838705449444022652355565392171702345881427096566633769494957447420015296687812138177576466001557317056675111027221005969582058022899529333118501380166134607864676483828739116173461269178580186257490266486677839206143742952162243351494227378653938710593503436239164822135914417914190306326552366654989657047816161866088059657348484650208804648917587381647596311004763609009433923628807524614747087370907674848755682961586688315674280522685036343187379852233394640325214899081294504832057011229815959420037782873168548447320460610743719611348921807017679017481761450571271353121504538616599488981500090579800223920074190259243090373197975821900780700994983554220578443939059789455319157185586612934190457927556018513712845810999355955231047286405348577289698269949529315641676747401507179920683528906096656269865346604825245447613068900673228624597839983919623198678889246997823457303425538250074268963180449541718530763258905302809600007299944411273014987193501682320824514373693134713866527503191580073279143086275003713746690240664855814859039455876481938038239569220725678019039050480876390746831297254406921453270519267262507843820232264191737352673268925464180832643899338691638282218257385606257475776691059059255302303114445822454278600219173720763694867106875068457113502491500388073504656799152135200251581518256215104764399515301707283274754264151543807825364161450197108879883727387093477427770004354318482968886709591946818257266432018518668134005188950818936559490195651342132066807456183872268255020846456930757669626740368630473237568715189840731662896998327481598778409201158765383448914364093919235518500273991995313625439096723754872384506907411868540620101022260019920486730850164320257564380330469491975531388141021789624314602105976973474026654086478535953344727481858929880747213733511596028875230104172769919771254751076195477658238344543363620834339799493240979523682870604654974849807411458413970564431884272290785767041903645182376449237883070663106400054251118347592277048642471665850924191188071391188795617326279324544211665128645710824853683627205877921300176381646070686087465015189344127757236896514029243563980383479813573936253276755173117350734421524872428449939741005549450504235910411855579757233304417120352975265436957913138078770206426593236938077956476982936814123774536338991098653758589247087558267603817517200390767537146533680698510122118199916051754470078537238491169553359792229740918073741384817330552101229726860709591659018519799482781149265985004923079601834415995143876094479546972166462535851542643215260243141498224867577987788423766186348687317679115018525104558716345706749942890553933642650461618674671154556006755314390616549147093936804564986443463961450438220362152406762190515061823854149008437459939334182355104574856378203866544401000626238988568308977840150220171310744124565624246900478266970170867838195019777202546995092582751359519995005632488038116545366585729919917509021256056617930088346818246573722242278351202844467835535078947626466439417333092836098812554627989117607752545931702872303942308409649609541986879146218441452717032910609434831215306455063339901653706420056993908069607050862479753834786944380384807128177688568002157423912412284060326246610084680338789899668589451070097117651298167403754077408452603311106679269461516981669288627428528214985766284440659354036167812199161489266566736683801438390018297720643002232031866138861219487931264851019071593248506045777980832084764662336685649221969889059160428833116253588012280798203184065757408956940520408997184425057879282238950799253433771440870506862193781343867894277617920304811869690899227908547152726181311361021942187101849272547858549820527191290014454746676308089316055111376988866151778299477802926255655650697478276694050540148953139848340830296268047498950
                     in ('(' : shows t ")", t, 0.0)
                   ]
       prop "roundTiesToEven" $ testUnary (roundTiesToEven . fromRationalR) cases

    let cases :: [(String, Rational, Double)]
        cases = [("0x1.ffff_ffff_ffff_f8p1023", 0x1.ffff_ffff_ffff_f8p1023, maxFinite)
                ,("(0x1.ffff_ffff_ffff_f8p1023 + 1/723)", 0x1.ffff_ffff_ffff_f8p1023 + 1/723, 1/0)
                ,("(0x1.ffff_ffff_ffff_f8p1023 - 1/255)", 0x1.ffff_ffff_ffff_f8p1023 - 1/255, maxFinite)
                ,("0xdead_beef.8p-1074", 0xdead_beef.8p-1074, 0xdead_beefp-1074)
                ,("0xdead_beef.9p-1074", 0xdead_beef.9p-1074, 0xdead_bef0p-1074)
                ,("-0xdead_beef.7p-1074", -0xdead_beef.7p-1074, -0xdead_beefp-1074)
                ,("-0x0.8p-1074", -0x0.8p-1074, -0)
                ,("-0x0.80007p-1074", -0x0.80007p-1074, -0x1p-1074)
                ]
    prop "roundTiesTowardZero" $ testUnary (roundTiesTowardZero . fromRationalR) cases

  describe "Float" $ do
    let proxy :: Proxy Float
        proxy = Proxy
    prop "fromIntegerR vs fromRationalR" $ eachStrategy (prop_fromIntegerR_vs_fromRationalR proxy)
    prop "fromIntegerR vs encodeFloatR" $ eachStrategy (prop_fromIntegerR_vs_encodeFloatR proxy)
    prop "fromRationalR vs encodeFloatR" $ eachStrategy (prop_fromRationalR_vs_encodeFloatR proxy)
    prop "fromRationalR vs fromRational" $ prop_fromRationalR_vs_fromRational proxy
    prop "result of fromIntegerR" $ \x -> prop_order proxy (fromIntegerR x)
    prop "result of fromRationalR" $ \x -> prop_order proxy (fromRationalR x)
    prop "result of encodeFloatR" $ \m k -> prop_order proxy (encodeFloatR m k)
    prop "add_roundToOdd" $ forAllFloats2 $ prop_add_roundToOdd proxy

    do let cases :: [(String, Rational, Float)]
           cases = [ let t = 20113311130255 % 822127761653273855988822146978202976557090789271144163906483851513046701868339517444102604474616762490976436939594169664101896669409817473587913461546435532885567073887954501607977104895740769882295378286300234464764201845440572849224022844453347299057834829757872072616746710668820893729486742297607776797874
                     in ('(' : shows t ")", t, 0.0)
                   ]
       prop "roundTiesToEven" $ testUnary (roundTiesToEven . fromRationalR) cases

    do let cases :: [(String, Rational, Float)]
           cases = [ ("0x1.ffff_ffp127", 0x1.ffff_ffp127, maxFinite)
                   , ("(0x1.ffff_ffp127 + 1/723)", 0x1.ffff_ffp127 + 1/723, 1/0)
                   , ("(0x1.ffff_ffp127 - 1/255)", 0x1.ffff_ffp127 - 1/255, maxFinite)
                   , ("0xbeef.8p-149", 0xbeef.8p-149, 0xbeefp-149)
                   , ("0xbeef.9p-149", 0xbeef.9p-149, 0xbef0p-149)
                   , ("-0xbeef.7p-149", -0xbeef.7p-149, -0xbeefp-149)
                   , ("-0x0.8p-149", -0x0.8p-149, -0)
                   , ("-0x0.80007p-149", -0x0.80007p-149, -0x1p-149)
                   ]
       prop "roundTiesTowardZero" $ testUnary (roundTiesTowardZero . fromRationalR) cases
