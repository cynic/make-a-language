module DAWG_benchmarks exposing (..)
import Array
import Benchmark exposing (..)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import DAWG exposing (..)
import DAWG.Simplify
import DAWG.Simplify3

suite : Benchmark
suite =
  let
    ten_words =
      [ "campstools", "oxidiser", "deoppilate", "straky", "reinvites", "colorizations", "kumbi", "broomshank", "tricolor", "moit"
      ]
    fifty_words =
      [ "automaticity", "heartsomeness", "cisterna", "barometz", "unhealing", "rat's", "gloea", "drepanoid", "klystrons", "annulettee" , "sphenotemporal"
      , "bromauric", "lecithin", "digitonin", "briefnesses", "tungstite", "signaling", "yclad", "astrophysicist's", "endoprocta", "myelobrachium"
      , "ceraunoscopy", "iconoclast", "nictitated", "dibbles", "stonewalls", "unpriced", "salal", "cephalothin", "mainlines", "indoctrinize", "poetries"
      , "pericowperitis", "registers", "veterinarian's", "oslo", "plural", "subchiefs", "thyrsoidal", "estranged", "wigwagged", "timeless", "chasselas"
      , "unfancy", "unrecurrent", "pikakes", "fistic", "raptorial", "unmisled", "bacterise"
      ]
    hundred_words =
      [ "consonantic", "bathyseism", "cenchrus", "glasshouse", "banquette", "anaxonia", "amplidyne", "crapefish", "peculated", "alcoholometry", "postamputation"
      , "transmutatory", "awkwarder", "nicknamers", "characterism", "blitz's", "tinker", "piddled", "proexpert", "byliner", "pabulous", "mian", "methodistically"
      , "harare", "monomorphemic", "underzealot", "vinaigrette", "sharewares", "flattens", "oceanologies", "navaid", "sambucus", "troglodytal", "dialytically"
      , "drolled", "apanages", "masticic", "deipotent", "slangishly", "cradleside", "toxicaemia", "talpacoti", "detasselled", "illatively", "gaspar", "rheostat"
      , "quinch", "tripersonalism", "fragilely", "voicelessness", "commenting", "sarangousty", "syncytial", "mating", "mannikins", "macusi", "calothrix", "sool"
      , "heppest", "calcaneotibial", "monasticism", "coachlet", "schul", "mericarp", "impersonalizations", "chitterling", "perigynies", "boastful", "supernature"
      , "haku", "fervors", "interdictory", "mudweed", "surtouts", "roud", "alimental", "nothingly", "sillabub", "pseudoscopic", "colectomy", "extricated"
      , "formicicide", "makaira", "remixt", "splenocleisis", "shivey", "bergamask", "hunky", "flagless", "internetted", "omophagous", "asteriated", "unheedfully"
      , "yquem", "spenses", "coremium", "calcined", "toppling", "repopularizes", "reconfining"
      ]
    five_hundred_words =
      [ "kindliest", "misrecollection", "photochlorination", "gassier", "paryphodrome", "amazements", "tumuluses", "spectroradiometric", "reflooded", "countries"
      , "pittas", "heptastrophic", "prideful", "confess", "heist", "boglander", "ringgiving", "synthesis", "temporises", "sclerocornea", "stipiturus", "czarevitch"
      , "toxicodermia", "bellweed", "hemophobia", "prefriendly", "anthocyans", "wanderable", "banteng", "pretrials", "valvelets", "prohuman", "mafiosi", "polyzoan"
      , "bihamate", "verisimilar", "alloy's", "rerigged", "bogmat", "omphalorrhexis", "chicquing", "nonpenal", "palls", "piperidide", "centage", "syncrasy"
      , "rationalisticism", "whitehead", "zesty", "diactinism", "linchbolt", "complementally", "ephestia", "canceler", "depended", "cicisbeisms", "muslim"
      , "cassandra", "dissembling", "psithurism", "unitarily", "moorish", "lieve", "needn't", "ashrafi", "alcaldeship", "isovalerone", "unparagonized", "climbers"
      , "whirlpools", "trimetalism", "nicolo", "underprivileged", "stereomicroscopically", "gnomologic", "vindicated", "myocele", "beseeches", "phaius", "edit"
      , "transferase", "starving", "pterygomandibular", "waysliding", "tympanum's", "compline", "leoparde", "indisputability", "rerob", "unconventional", "siliqua"
      , "drawliest", "florisugent", "meditativeness", "maremme", "jointweed", "abberations", "sleepful", "misproud", "facund", "shadowbox", "unhazardous", "nocturn"
      , "cooke", "fanglement", "entrainer", "ribalds", "scolopacidae", "designer's", "cowperian", "bacillemia", "pteroclomorphae", "gurgeons", "proenzymes"
      , "limelighter", "bedevilment", "adead", "stoutnesses", "unstabbed", "oreotrochilus", "pollinations", "caprifig", "obsessiveness", "unnurtured", "microdiactine"
      , "philathletic", "lessen", "flameless", "windlestraws", "unrecruitable", "subbrigade", "leucin", "explanted", "tailorings", "albertinian", "glycogens"
      , "reconcentrated", "monomolybdate", "kuhnia", "vulvae", "gleets", "turtan", "nonrecognized's", "conundrumize", "woorali", "anchored", "dareall"
      , "seroconversions", "unhappen", "ethicalities", "putid", "butoxy", "cursoriidae", "authigenic", "hirpling", "leisterer", "delaware", "microspectrophotometers"
      , "welk", "underwriting", "adscript", "mischievousness", "pericu", "preexcite", "dysgeneses", "sanctimonies", "externality", "niobate", "aprosopous", "caring"
      , "harmed", "antipopulationist", "bureaucrats", "scotodinia", "passionals", "autoluminescence", "nauplius", "countertally", "reabsorbing", "geomorphologies"
      , "campstools", "oxidiser", "deoppilate", "straky", "reinvites", "colorizations", "kumbi", "broomshank", "tricolor", "moit", "obsecrated", "undergarb"
      , "unobumbrated", "untribal", "eaglewoods", "ulothrix", "china", "raunch", "unflaky", "incorrespondent", "spiritualizations", "perorations", "theologizes"
      , "owllight", "mechanomorphic", "upbow", "beddings", "disproportionality", "polytrichous", "contusioned", "mustarder", "unenterprise", "elaeoblast"
      , "splanchnesthetic", "davening", "posies", "cotylosaurs", "romanticality", "biostatistic", "ferias", "bespousing", "quinizarin", "zonetime", "antidynastic"
      , "decanonization", "ratels", "coprocessors", "maximizers", "antierosion", "swifty", "rheobases", "coequated", "tolerators", "androgynous", "mouses", "sunnahs"
      , "aggregatae", "synoviparous", "insipidities", "phalangistidae", "disattune", "dullification", "palaeontographical", "runed", "opiniastrety", "polyphage"
      , "unbeginningly", "pecan", "vegas", "quietist", "synaestheses", "henequen", "apparats", "sarasota", "demerits", "welders", "autopathic", "piproid"
      , "pseudometallic", "antiberiberin", "panimmunity", "impapyrate", "reborrow", "electrodynamism", "neighborlike", "bicycles", "sialagogues", "unbrooch"
      , "goddizes", "limacinid", "kiteflier", "frigorie", "forevalue", "masooka", "spongospora", "sambar", "lightfulness", "patriarchic", "adarme", "dervishlike"
      , "jasione", "recriminations", "bonzery", "arboresque", "pindarus", "hyponastic", "poephagous", "barish", "holytide", "cockneyish", "cariniform", "diapasm"
      , "redoubles", "stylise", "footlights", "calorigenic", "regulating", "southernliness", "neuroanatomic", "odorizer", "rimbaud", "unpranked", "nonillumination"
      , "split", "loudspeaker", "coloraturas", "unio", "melicerous", "roomies", "coumaric", "precedencies", "wobbegong", "anystidae", "menders", "rosabel"
      , "nonmatriculated", "hypsographic", "benniseed", "pyche", "pilifer", "scenery", "persuasions", "novelist", "pedanticness", "unacquired", "bronchopleurisy"
      , "easies", "tychite", "agriculture", "lithophyl", "diquat", "outdating", "lavable", "contented", "gnomic", "sinistruous", "pronegroism", "antagonizations"
      , "balei", "lustihoods", "glassweed", "overgreasiness", "starlet's", "infragular", "orators", "cist", "foraying", "olivescent", "orientalism", "equidae"
      , "centrifugalizations", "seattle", "malshapen", "desalinator", "unhuntable", "symbolizer", "chainwheel", "bombshell", "krameria", "toponymical", "alert"
      , "fidation", "manganins", "somnify", "assonances", "lockatong", "amarine", "empowering", "staticproof", "daub", "undemanded", "finically", "fondu"
      , "mayoresses", "seashore", "mailbox's", "subduers", "udaller", "quadricuspid", "neocyte", "outcropper", "pantothenic", "suggestion", "cornific", "colluviums"
      , "penumbra", "relaxednesses", "tearlessly", "alewives", "tolan", "boyne", "lifeworld", "buzzword's", "prostatocystitis", "midget", "inaffectation"
      , "counterscalloped", "rhysimeter", "prostrations", "reversive", "unchannelled", "astomatous", "belch", "peridermium", "lymphosarcomatous", "offensively"
      , "nondiversification", "reflowered", "retrenches", "euprepia", "bogglers", "audibertia", "anastigmats", "irreflective", "hysteric", "uncultured", "qiviuts"
      , "glossina", "chatelaines", "misaunter", "sorels", "peto", "employment", "kampuchea", "timbersome", "titlark", "undefinedly", "phrenogram", "catty"
      , "prefinancial", "hydroxylizes", "seamen", "sonnet's", "tumour", "dogbane", "quindecemvir", "affectless", "achylous", "huntresses", "rangler", "equablenesses"
      , "initialist", "typhlostenosis", "coudee", "vestigial", "heliocentric", "turtlebacks", "sympathizings", "ganofs", "upspring", "protector", "defterdar"
      , "ecotages", "beden", "dermatoses", "blowsy", "combating", "simpletons", "torpedoer", "carbolineum", "lysogenization", "popeship", "neritidae", "imploding"
      , "mollifiable", "isostemony", "cholelithic", "undeluded", "unwire", "chilver", "uncontrite", "unlean", "jaalin", "endosmotic", "coccogone", "cotland"
      , "gestatorial", "balai", "arline", "coatrack", "personalize", "tanginesses", "uncapably", "misadventured", "chaetognatha", "enchondromatous", "plaices"
      , "anniversarily", "nudity", "shoreward", "knorria", "protospore", "inquirendo", "cofferlike", "flexor", "unsupporting", "orthodoxal", "aphodian", "diaphonia"
      , "calque"
      ]
  in
    describe "DAWG"
      [ --describe "(Original Simplify) fromWords" <|
        --[ benchmark "50 words" <|
        --    \_ -> fromWords fifty_words
        --]
        -- describe "(Simplify3) fromWords" <|
        -- [ benchmark "50 words" <|
        --     \_ -> fromWords3 fifty_words
        -- ]
        -- describe "(Simplify) fromWords" <|
        --   [ benchmark "50 words" <|
        --       \_ -> DAWG.fromWords fifty_words
          -- , benchmark "100 words" <|
          --     \_ -> DAWG.fromWords hundred_words
          -- , benchmark "500 words" <|
          --     \_ -> fromWords4 five_hundred_words
          -- ]
        describe "(Carrasco & Forcada) fromWords, algorithm only" <|
        [ benchmark "10 words" <|
            \_ -> DAWG.Simplify3.toMADFA ten_words
        -- , benchmark "50 words" <|
        --     \_ -> DAWG.Simplify3.toMADFA fifty_words
        -- , benchmark "100 words" <|
        --     \_ -> DAWG.Simplify3.toMADFA hundred_words
        -- , benchmark "500 words" <|
        --     \_ -> fromWords4 five_hundred_words
        ]
      , describe "(Carrasco & Forcada) fromWords, + DAWG transform" <|
        [ benchmark "10 words" <|
            \_ -> DAWG.Simplify3.fromWords ten_words
        -- , benchmark "50 words" <|
        --     \_ -> DAWG.Simplify3.fromWords fifty_words
        -- , benchmark "100 words" <|
        --     \_ -> DAWG.Simplify3.fromWords hundred_words
        -- , benchmark "500 words" <|
        --     \_ -> fromWords4 five_hundred_words
        ]
      ]


main : BenchmarkProgram
main =
  program suite