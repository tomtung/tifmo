
import tifmo.proc.preProcEnglish
import tifmo.proc.mkSTreeEnglish
import tifmo.proc.addknowEnglish
import tifmo.stree.InferMgr
import tifmo.knowledge.EnWord
import tifmo.knowledge.EnConfiFunc

//val traw = """"I will take a brief vacation with some priest friends after Christmas and then I will go on retreat at a monastery," Law, reading from a brief statement, told reporters."""
//val hraw = "Law said he plans to take a brief vacation after Christmas and later retreat to a monastery."

//val traw = "No animals were harmed in the making of this film."
//val hraw = "The movie shooting didn't hurt my dog."

//val traw = "Hurricane Isabel was a tropical storm when she entered Virginia, but caused damage to 75% of the state, making it one of the costliest disasters in Virginia's history."
//val hraw = "A tropical storm has caused significant property damage or loss of life."

//val traw = "Olympic officials familiar with Salt Lake City's bid for the 2002 Winter Games have described in recent weeks a process by which Salt Lake City Olympic officials targeted the votes of 17 African IOC members after losing by four votes to Nagano, Japan, for the right to play host to the 1998 Winter Olympics."
//val hraw = "Before Salt Lake City, Winter Olympic Games took place in Nagano."

//val traw = "Arabic, for example, is used densely across North Africa and from the Eastern Mediterranean to the Philippines, as the key language of the Arab world and the primary vehicle of Islam."
//val hraw = "Arabic is the primary language of the Philippines."

//val traw = "A jet filled with tourists returning home to the French Caribbean island of Martinique, crashed Tuesday, in Venezuela."
//val hraw = "A plane carrying vacationers home to the island of Martinique crashed, Tuesday, in Venezuela."

//val traw = "If you suffer from certain genetic illnesses, or carry a gene for that illness, scientists can take one or two cells to check for those genes and transfer only healthy embryos."
//val hraw = "Scientists can determine whether an embryo has genetic illnesses or is a carrier of them."

//val traw = "On 12 August, the San Carlos Battalion came across mines placed in their path and one soldier was killed while two were seriously injured. Meanwhile on 10 August, urban commandos took a patrol car by surprise and dropped a grenade inside the car, injuring four and partially destroying the vehicle."
//val hraw = "Four people were injured by a grenade."

//val traw = "The Truth and Reconciliation Commission's investigators submitted the preliminary results of their work in early October to around 200 organisations and individuals to warn them of charges against them and to give them the opportunity to respond before final drafting of the report."
//val hraw = "The Commission investigated 200 people and oragnisations, before charging them, in order to prevent them from responding prior to the official release of the report."

//val traw = "The Damascus public have been welcomed into the election candidate's private homes or to specially erected tents where voters have been enjoying food, drink and entertainment, particularly in the home of the wealthy candidate, Adnan Mullah, where dozens of supporters and friends have been invited to dinner every evening."
//val hraw = "Candidates are entertaining voters in their homes."

//val traw = "The council prohibited any travel outside of Haiti by all officers of the Haitian military and police and all major participants in the September, 1991, coup."
//val hraw = "The council is prohibited to travel outside of Haiti by military officers."

//val traw = """"Beatrice and Benedict" is an overture by Berlioz."""
//val hraw = """The program will include Falla's "Night in the Gardens of Spain," Ravel's Piano Concerto in G, Berlioz's Overture to "Beatrice and Benedict," and Roy Harris' Symphony No. 3."""

//val traw = "To the world, M. Larry Lawrence, the new U.S. emissary to Switzerland who hosted President Clinton on his Southern California vacation, will be known as Mr. Ambassador."
//val hraw = "Larry Lawrence is the head of the U.S. Embassy in Switzerland."

//val traw = "Jack Straw, the Foreign Secretary, will meet his Brazilian counterpart, Celso Amorim, in London today."
//val hraw = "Jack Straw is a partner of Celso Amorim."

//val traw = "The watchdog International Atomic Energy Agency meets in Vienna on September 19."
//val hraw = "The International Atomic Energy Agency holds a meeting in Vienna."

//val traw = "The watchdog IAEA meets in Vienna on September 19."
//val hraw = "The IAEA holds a meeting in Vienna."

//val traw = "Angola as a Portuguese colony achieved independence in 1975."
//val hraw = "Angola became independent from Spain in the 1970s."

//val traw = "Angola as a Portuguese colony achieved independence in 1975."
//val hraw = "Angola became independent from Portugal in the 1970s."

//val traw = "Anyway, maybe it's best not to plan everything."
//val hraw = "Don't plan everything."

//val traw = "The sale was made to pay Yukos' US$ 27.5 billion tax bill, Yuganskneftegaz was originally sold for US$ 9.4 billion to a little known company Baikalfinansgroup which was later bought by the Russian state-owned oil company Rosneft ."
//val hraw = "Baikalfinansgroup was sold to Rosneft."

//val traw = "The sale was made to pay Yukos' US$ 27.5 billion tax bill, Yuganskneftegaz was originally sold for US$9.4 billion to a little known company Baikalfinansgroup which was later bought by the Russian state-owned oil company Rosneft ."
//val hraw = "Yuganskneftegaz cost US$ 27.5 billion."

//val traw = "Often criticized and blamed for politically unpopular policies, and confronted with numerous setbacks, the World Bank is experiencing a difficult time with regard to which strategies to adopt, in particular in Africa."
//val hraw = "The World Bank is criticized for its policies."

//val traw = "He endeared himself to artists by helping them in lean years and following their careers, said Henry Hopkins, chairman of UCLA's art department, director of the UCLA/Armand Hammer Museum and Cultural Center and former director of the Weisman foundation."
//val hraw = "The UCLA/Hammer Museum is directed by Henry Hopkins."

//val traw = "Euro-Scandinavian media cheer Denmark v Sweden draw."
//val hraw = "Denmark and Sweden tie."

//val traw = "Binge drinking among young women is on the rise, bringing with it a number of health consequences, including fetal alcohol syndrome."
//val hraw = "Young women are binge drinking more, which can lead to the health risk of fetal alcohol syndrome."

//val traw = "Other wildlife, such as gemsbok, zebras and springbok, are also dependent on the Ugab wetlands."
//val hraw = "Zebras depend on the Ugab wetlands."

//val traw = "A two-day auction of property belonging to actress Katharine Hepburn brought in 3.2 million pounds."
//val hraw = "A two-day auction of property belonging to actress Katharine Hepburn brought in £3.2m."

//val traw = "Nichols, 49, was convicted in the April 19, 1995, bombing of the Alfred P. Murrah Federal Building in Oklahoma City."
//val hraw = "Nichols was sentenced to life in prison for the April 19, 1995, bombing of the Oklahoma City federal building."

//val traw = "Sharon sent dismissal letters to Benny Elon and Avigdor Lieberman, who oppose his withdrawal plan, on Friday."
//val hraw = "On Friday,Sharon fired Benny Elon and Avigdor Lieberman."

//val traw = "Moscow (USSR), 12 Jul 89 (PRAVDA)- Blood is flowing in Columbia, where last year, according to official statistics, there were 4,600 victims of political violence, including Jaime Pardo Leal (president of Colombia's national coordination committee), around 30 deputies and many municipal advisors and mayors."
//val hraw = "Jaime Pardo Leal was killed in Moscow."

//val traw = "Across the continent, voters voiced discontent by casting ballots for opposition and fringe parties. But most of the 350 million EU citizens eligible to vote didn't bother to even cast a protest ballot in the last European elections."
//val hraw = "Most EU citizens voted in the last European election."

//val traw = "For women who are HIV negative or who do not know their HIV status, breastfeeding should be protected, promoted and supported for six months."
//val hraw = "For HIV-positive mothers, the decision about whether or not to breastfeed a child can be difficult."

//val traw = "Juan Antonio Samaranch, president of the International Olympic Committee, left Lillehammer for Sarajevo to pay tribute to the besieged host city of the 1984 Winter Games"
//val hraw = "The International Olympic Commitee's head office is in Lillehammer."

//val traw = "A great earthquake occurred at 00:58:50 (UTC), at 6:58 a.m. local time, on Sunday, 26 December 2004. The magnitude 9.0 event was located off the West coast of Northern Sumatra."
//val hraw = "An earthquake occurred on the east coast of Hokkaido."

//val traw = "The U.S. Senate has scheduled debate and a vote in June on a bill that would allegedly initiate a process for Native Hawaiians to achieve the same level of self-governance and autonomy over their own affairs that many Native American tribes currently have. Critics of the bill characterize it as going much further than any existing tribal recognition, creating a governing entity based solely on race, without the same requirements as needed for Native American tribal recognition, such as having existed predominantly as a distinct community, having exercised political influence over its members as an autonomous entity, and have continuously been identified as a tribal entity since 1900."
//val hraw = "The Shoshones fight for their rights against the U.S. government."

//val traw = "According to Dasgupta, these rights made Iraq a leader in equality of the sexes in the Middle East for the better part of last century, although a number of studies reveal horrific abuses of both women and men, under Hussein's regime, and Hussein enabled laws allowing men to kill their wives in certain situations (see Wikipedia article Honor killing for background on the practice). The first Gulf War in 1991, and ensuing sanctions, made economic conditions in Iraq difficult, and literacy and employment rates of women began falling."
//val hraw = "Women in Iraq have lost their freedom."

//val traw = "Jessica Litman, a law professor at Michigan's Wayne State University, has specialized in copyright law and Internet law for more than 20 years."
//val hraw = "Jessica Litman is a law professor."

//val traw = "Ralph Fiennes, who has played memorable villains in such films as 'Red Dragon' and 'Schindler's List,' is to portray Voldemort, the wicked warlock, in the next Harry Potter movie."
//val hraw = "Ralph Fiennes will play Harry Potter in the next movie."

//val traw = "The National Park Trust identified 20 high-priority sites - including the Blue Ridge Parkway in North Carolina and Virginia and Everglades National Park in Florida - as areas with private property that could be sold."
//val hraw = "Everglades National Park is located in Florida."

val traw = "No animals were harmed in the making of this film."
val hraw = "The movie shooting didn't harm my dog."

val tstree = mkSTreeEnglish(preProcEnglish(traw))
val hstree = mkSTreeEnglish(preProcEnglish(hraw))

println("============ Test Start =============")
println(traw)
println(tstree)
println(hraw)
println(hstree)

val imgr = new InferMgr(hstree)
imgr.addPremise(tstree)

val tws = tstree.streeNodeList.map(_.word.asInstanceOf[EnWord]).toSet
val ws = hstree.streeNodeList.map(_.word.asInstanceOf[EnWord]).toSet ++ tws
addknowEnglish(imgr, ws)

val tr = imgr.trace(new EnConfiFunc(tws), 0.1, 9)

tr.foreach(println(_))

sys.exit(0)
