package aventofcode

object Day2 {

    def main(args: Array[String]): Unit = {
        println("Day 2 !")
        print("Challenge 1 : ")
        println(countValidPasswordChallenge1())
        print("Challenge 2 : ")
        println(countValidPasswordChallenge2())
    }

    def loadRessources(): List[String] = {
        //List("2-7 p: pbhhzpmppb", "3-6 h: jkhnhwhx", "5-6 x: xxxxxmxf", "10-11 r: rrrrrrrrrrkr", "2-7 n: mcfmxnnnwnztnkrqdwd", "1-2 t: ssgt", "5-7 l: llnlklll", "4-18 b: hfbzbbbbbbbhbbbbbbbb", "15-18 x: xxxxxxxvxxxxxxpxxx", "5-8 k: kmlkkkgk", "12-13 l: lllllllljlplqlll", "1-13 m: hmmmmmmmmmmmxm", "2-4 z: wzzz", "5-13 r: rrrrrrrrrrrrdrr", "2-13 k: wkbwczdmrgkklvxpppfx", "2-5 h: lhhhvw", "7-8 l: fkvkgltl", "1-6 w: wwwwwwwwwl", "4-5 p: ppppgw", "4-5 k: kkkkkqk", "2-5 t: tjttttt", "1-10 k: kkkknqxfszj", "14-16 j: jfjnbjmttjvwkjhq", "1-4 h: hhhhhhhhhhh", "1-6 t: jglbqlwjwgkcgkrh", "1-4 b: lbbbb", "12-13 t: thtttttttttvq", "1-4 j: zgsjn", "8-11 l: rllltlllslllbl", "5-7 h: hhmhhhhhhhjhhhhhhhh", "3-13 m: mfmmntkhbrswr", "3-5 p: kcppn", "8-9 s: ssssssssh", "5-6 t: ttlchw", "1-6 d: dddkdgdddddddjdd", "11-14 n: nntnjpnnwnqnfgnnnnn", "2-8 p: pqpppprddpp", "9-10 k: kcgkkkkkkk", "5-8 d: drddddpdd", "3-4 c: kgcvcw", "1-12 q: gqfqwqqqqqqqbjq", "10-15 z: zfzzzzzzzmdzznjjzpz", "1-5 p: cppspppgpspbvj", "14-20 c: cccvcccccccrcccccpcc", "4-5 j: jjjjq", "5-7 f: vpgzrndzvgdrrfhbgbz", "3-6 f: ffmzxkff", "16-17 t: ttttthtttttttttttt", "5-7 x: xbxxxxxxl", "3-6 v: vhglzvvvd", "3-5 n: qnndv", "7-10 q: nhqsqnqlhqqqzqqxgq", "3-5 f: fnfrd", "3-12 l: jgrzdzvqbrllsljfl", "15-18 h: hfvhhhhhhxhhhhhhhhh", "4-6 j: cjjjjjjz", "1-3 k: pgkc", "10-16 n: nnnnnnnnnnnnnnnnn", "10-11 n: nnnnnnnndmln", "2-7 n: ljwspmb", "3-12 r: brrrrzrfqjprsrngvjw", "2-3 g: ggzg", "11-12 b: qvbbvmxbbbhb", "1-5 f: ftvdbzf", "16-17 s: ssspskbsssssssszmsss", "3-4 x: fxxfxxt", "3-7 c: jscxgnm", "6-9 s: spqsstbscrbhbsb", "11-12 n: nnnnnnnnnnnmn", "3-11 r: rrjkrxrsrbrncbprr", "3-6 r: rrrrrrrr", "13-15 j: jjjrrjjjtjjjjjjjj", "8-20 g: gggggggzgggmgggggggn", "14-16 l: llslllllllllllbkfl", "4-10 l: tpgxptlqxlksp", "11-12 w: wwwwwwwwwwbr", "1-8 q: qqqqqqqqq", "11-13 c: ccjccccctcccw", "1-9 s: nmssmsssskhsss", "6-9 x: xxxxxxvxlxx", "10-13 h: hhhhhhhcccksph", "8-9 j: jjjjjjjvhj", "7-9 d: znddxddlb", "2-11 s: nmsmssssrcs", "5-6 c: cccccxgc", "5-11 j: ndqbqgjjdjjmj", "4-13 g: gxjmwgkzxgggmg", "4-5 g: ggggkggg", "7-10 n: dnnpnnnnznn", "4-5 k: ghkwkck", "4-11 z: zzbszztzmzf", "8-10 p: vmpvspxnhs", "9-14 z: zzzxzzlzzzzzzd", "1-4 w: vwhphw", "9-10 w: tswwgwzlww", "2-5 n: vdbnnljmwfjwknhd", "4-5 m: mpmxmm", "12-19 d: dddddddddddddddddddd", "8-11 c: bcccwnfdnwjgccbwcjc", "9-11 q: jwzfskvscqj", "11-13 m: pmmmmfmmmmhmftmmmmp", "1-2 c: lcxcbdhcmscj", "17-19 j: jjjjjjjjjjjjjjjjxjwj", "7-9 f: ffffffgfz", "3-4 b: bfbb", "18-20 q: spxfrghllqxwrgjrzqwf", "1-3 w: wwwww", "10-11 j: jjjjjjkjjjr", "5-10 n: mhqhwflnpr", "3-5 q: qtsqk", "4-7 t: ttkthrtfdt", "6-7 r: rrrrrlgrr", "8-10 m: jmmmmzmmmdqwmlqlbdvs", "10-13 n: ngqntnnnnbnnhzn", "5-8 m: jdcmjmmt", "8-13 h: dmgkbdhbhwphh", "11-13 q: ppjwqnqqqhfqqdgzgqx", "2-4 b: bvbvbq", "9-10 j: jjjjjjjjzmjjjj", "5-10 k: kzkkkkkkkqk", "6-8 b: bbbbbbbsb", "4-16 d: tvgdsqhnldtsdtxj", "6-8 n: dndnnznndk", "5-6 s: srlclsqvkbr", "2-4 x: xbxsx", "1-9 g: hgggggggrggggg", "7-9 n: nnnnnntnnncnnlnnnnnn", "7-10 x: wxxhfxslxwdlhxvmxqt", "14-16 k: ckmrzcktfjqlqdxx", "5-11 l: mllljlkjllpzl", "7-11 s: sstsksmfsks", "3-4 s: rkxx", "3-5 n: rnhnnn", "7-8 j: jjjjbjgw", "2-4 b: lsgb", "5-13 d: ddjdxddlddgdvd", "12-13 g: zrjgggdzgbxcr", "10-11 p: ptphppcsppppcpspsv", "6-10 x: zdxxxwxxxxpxxs", "13-18 v: bvvvgqvvvvvmvvtvfvvv", "2-4 j: djwbx", "10-14 c: pcccczpcfccccc", "11-12 x: xxfdpxqgzjtxjfr", "1-12 t: rtttttttttttttttttq", "3-5 f: fvnsffl", "3-12 p: jdvpwgtmsmmphcl", "6-9 s: sssssrsss", "7-14 c: ccccrcgccccfkcxz", "12-13 q: qhftwgcslmlgjpm", "6-9 q: bqdqbqqsqrvgbmqbqs", "1-7 t: lmtcktrhgd", "3-4 m: mcmmmdmw", "8-9 g: gglggggghgggggggsggg", "6-9 b: bnbbbrbbfbbbb", "7-9 t: tttttttgpt", "3-6 h: hhshhqhhhhhhhhhhhh", "1-7 w: wtbwgpv", "7-9 t: tttqctktbbtt", "5-6 s: pnwzskmh", "10-11 v: dfpdvxxrxpvv", "15-16 k: qkkkkkkkkkkkkkkkc", "1-3 s: msvss", "13-14 g: gggggbgfggggggk", "2-4 b: xbfbqlqxcxb", "5-7 s: brksswcsd", "8-12 z: zfrstgvpwgtzz", "5-8 j: jjkrjhjjvkxngrqklnvm", "4-7 j: jjjjjjgj", "14-16 q: qdfnpmznngjjkqdqqcgx", "15-16 q: qqqqqqqqwqqqqqbf", "10-15 x: xsxxxxsrrxxxxxxxx", "10-11 h: hhhhhhhhhhhh", "4-8 l: qlllllls", "5-6 f: fffnft", "5-12 n: nlmfnzgvgnldnk", "3-10 q: vhbqdvjznvxstsqpnr", "16-17 g: gggggggggggggggqggd", "4-9 z: zzzczzzzgz", "8-10 w: wwwwwwwbwlw", "6-7 l: lfllllcl", "4-5 m: mmmdfm", "8-9 f: ffffgfffxffff", "4-7 g: ggggggwgg", "12-13 c: fcckcccqlccjpccc", "16-17 g: ggggggqphggggggnggg", "2-10 v: jdpqlbxhcvxsw", "14-17 b: tbwgtxtjqdrjsbhfk", "2-3 r: rzrd", "12-18 n: nnntjcnvxnnbhjncsp", "4-11 d: gjqdlnlkhlvdp", "6-16 r: rfrpnprkqrrrdnrm", "4-6 x: dkxxgg", "8-9 x: xxxxxxxxj", "2-4 t: sxrtphmxfb", "17-18 g: mggggggggggggggggrg", "3-4 c: zqgdfc", "1-12 n: nnnnnnnnnnnhnnnnn", "5-7 t: tttttttt", "7-8 k: kkkkkkmb", "8-11 n: nnnnnfnnrgnnn", "6-8 m: jmkmpmxt", "9-14 t: vzhtgdkrtncjgz", "2-16 h: xhkxnrsdndhbfswwvfr", "14-16 z: czzfwzbvfwwfmzkbm", "3-4 p: pppt", "2-7 n: hglwdnds", "18-19 l: llllllljlpstjlmxllj", "4-9 z: hzxknghmqllg", "3-13 w: wbwwwwwwwrwwmfww", "12-13 q: qqqqqqqqqmqqqqp", "3-5 b: rbcbqb", "13-15 t: ttttttttttttgtv", "9-11 h: hhhhhhshfhhhhhhhhh", "4-5 d: ddsdd", "17-18 c: xcqcccfcccnscccccvb", "2-3 z: qgqc", "3-8 c: cpvsqnwxv", "3-11 r: zrgqrrrkvxhlh", "5-7 h: xvhbmhhg", "8-16 n: nnnknvnrfhntnphdnn", "12-13 k: kkkkkkkkkvkkt", "8-10 n: nhnnnndnnj", "2-8 h: zhgptrkxk", "1-6 m: mmmmmqmm", "3-4 g: gggggd", "4-7 k: kkwpsxkjkksk", "4-6 s: sssssw", "4-6 q: jqxgnk", "3-9 l: llzvtlllj", "2-4 s: stsxsssvs", "15-17 b: bbbbbbbbbbbbbbqbtb", "17-19 m: mmmmmmmmmmmmmmmmdmp", "3-4 w: wwww", "1-4 b: bbbbbtb", "10-16 b: bbbbbbbbbbbbbbbb", "16-17 d: dsksvpjsjhdzdfkdw", "8-9 s: ssjssssqssssssssssss", "8-12 g: gggvggggggwngw", "3-6 h: bhrphb", "12-18 n: nnnnnnnnnnnxnnnnnnn", "5-13 j: xjjjfjjcjjjjp", "2-3 c: clbc", "3-4 l: lllbl", "2-7 v: hncqbwvg", "8-11 n: nnnnnnnnnnt", "1-8 j: xjjjjjjjjj", "10-15 h: xhhnhhhhlhkhhhhhhh", "2-4 c: ccccc", "13-14 x: xxxfxxxxxxxxvxxxp", "3-11 h: hhhhhhrhgthhvxhz", "1-11 k: kvkwtwkkkkdd", "13-15 l: lllzllllllllllvlll", "6-8 b: bbbbbbbb", "4-15 b: bscbpbjlctwjdpmrzhth", "3-6 j: jjjjzj", "7-9 x: fsxxxnzbx", "11-12 t: ttntmctrttgzp", "2-5 r: rzrrrrr", "2-12 b: wmpnbbhlbltbbbbxz", "8-9 t: pxzzcttvt", "10-13 g: ggsghggwgjgbhgcgj", "7-8 m: mmmmmmxsw", "4-9 n: fnfqhnbpnqzqzn", "6-7 m: fmwmmmm", "2-4 g: grgb", "3-5 j: wjjcw", "2-4 p: kplpnhpprmzx", "12-19 w: wwwwlwwwwwwfwwwwwwww", "7-8 x: xxmxxxxx", "1-3 z: zfzzz", "8-11 q: cqdxsqqqnknn", "14-15 v: vvcvvvnmvvvfjqvv", "9-10 h: hhhhhhhhfw", "3-4 f: dsffnfjrsghfnrfctx", "5-12 x: xxxxhxxxxxxzxxxx", "9-14 l: llllllllrllllll", "10-13 w: wwwwwwwwwxwwbw", "4-8 g: gggphggg", "1-2 p: nppp", "1-5 m: fkjhdqmcbcmm", "7-8 v: vlvvvvxvvv", "1-13 q: qmjrfwkqpqpjmrpjjwh", "13-14 z: zzzzdzzzzmzzchz", "6-9 w: wthrljwwbw", "3-4 d: ndrdlnlxdxsvkdlmqpq", "1-2 l: cllllll", "1-3 v: spvx", "3-6 r: rrrvrjr", "3-4 m: hmdsm", "6-9 r: lxrrxdzrr", "8-11 b: snqbjkzbpzbpbgrgb", "1-4 k: kkfkndcllgz", "8-17 d: ddddddddsdwxdndddd", "1-4 w: wwwcw", "12-18 x: rxxbmvxmhqthxxwnxx", "17-18 v: vxvvvvvnvlvvvvvvvv", "5-6 z: zzzzfz", "3-6 z: fzlzpg", "4-7 w: wwhwwwwgfww", "7-12 h: hjhwhhhpzthj", "2-8 b: dbjbbbrbm", "2-8 b: fbjgcbbbrrnlbqbbqgp", "6-7 c: wscbccr", "2-18 x: xxnsrxxxxrgxxqxxxxr", "1-16 k: wpxkwkhkxswkqklpphdk", "4-7 q: qqqrqqqc", "11-15 x: xxxxxjxjxxxxxxlkx", "1-3 d: ddxkb", "7-8 b: bbqbbbkbb", "6-10 p: ppppphppptw", "12-14 x: xxvxvxxxxxxnxg", "8-9 q: qqsqqpqjq", "1-3 l: nljl", "17-18 f: ffffffffffffffffff", "5-6 z: czzcrzpzzkcv", "2-6 s: ssdsss", "11-18 m: nmmvmgmmmmkmmmmmmmm", "1-10 j: fjjjjjjhjj", "5-6 w: kwhzwtlwwrw", "14-15 v: vsvvvjvvvmvtpvvzv", "13-18 m: nmmprgbmhbmrmkfnwqvd", "5-11 x: xxxxmxxxxxnx", "5-10 m: mnmmfmmhmpmprhm", "5-13 x: xxxxwxxxxxxxvx", "2-12 q: lqhwhgbcvsnr", "8-13 k: kkkkkkvmkklkkkk", "5-13 g: ggmfntxjvpdgsggtc", "1-5 h: hhghhf", "13-15 p: pppgppppmfpnfpjpsdnp", "10-20 q: qqqqqqqqqbqqqqcqqqqw", "5-9 m: pldlmvgqnhxvmnmdwbgp", "1-3 r: vrmrnbr", "6-7 v: vvvvvvv", "4-6 x: xxxwxx", "5-7 w: lswpwwclww", "9-14 q: qqqqqqqqqwqqqqqq", "8-10 h: hqcclwxhhhbds", "18-19 c: cccccccccccccccccclc", "1-3 z: xzzz", "1-5 k: kkksl", "1-6 c: ctfcbg", "8-10 p: pppppppppwpppp", "7-8 z: ztzzzcwfzgz", "12-18 v: vvvvvvvvvvvvvvvvvqvv", "3-5 j: jjtjjj", "11-13 t: tttttttttttttt", "1-7 j: jjqfjprjjjjj", "11-16 l: llltllcklfqklnljl", "2-3 g: kwlgghsrrskhltrnv", "3-4 g: gggrnzfgq", "12-13 x: nxxjxxxjjxxcm", "3-7 z: pvhtqsdrnzvdscddpl", "1-10 m: tqjnhztzzbvsprhfdjr", "3-16 h: hhjhhhhhhhhhhhhhhh", "5-9 v: jkpmqvvzv", "3-4 g: gngs", "8-18 q: qqcqqqqzqqzqqqqqqqq", "12-16 d: ddgdddddzdddddmh", "9-10 c: nwcccccmcpcc", "9-10 q: qqcqqqqqlqq", "2-8 p: dfxpsdblhcpr", "12-13 m: lmmmlmvmvmmkn", "5-15 s: sssssssssssssskssss", "2-5 x: xcxgm", "1-10 w: lwnvwwzcwwtwwh", "8-13 h: hhhhhhhhnwhhhhhh", "8-15 m: zmmmcmsrmdmbmmmmnmm", "7-13 v: khjbvgsncvvfxhnmdv", "15-16 b: bbbblkkfsbwbbbbh", "3-4 m: dzmq", "5-13 k: gkwkkkvkrkkkkwkksvkk", "7-10 h: jhbhpwhhhhshkfrtfsh", "13-17 z: zpnzzzzlczzzczzzkzzz", "11-14 g: gggggggggglggg", "11-12 m: mmmmmmmmmmjmmmh", "17-20 p: pppppppppppppppppppp", "8-12 n: nkgnnnnnnnnbnn", "4-6 m: mfdxcz", "9-10 z: zzzzzzzzzzzzzzg", "1-6 p: sppppsggwx", "1-10 v: vvhvwvvbvl", "1-5 w: rwmwv", "4-5 b: bbbbz", "14-15 k: kkkkkkkkkkkkkkck", "6-9 h: hhhhhhhhhhmh", "4-5 v: vdvmv", "13-14 x: hxxrskxxtwxrqp", "5-12 c: zvczbchcxvcn", "7-8 j: jjjlwjtlj", "15-18 j: jjjjjjjjjjjjjjsjjjj", "2-6 r: brcnrcrrwr", "6-8 q: qzwqqxkx", "11-15 p: ppplppsfpppppppps", "6-7 g: ggpffgg", "10-11 q: bqqqfqmbqmq", "7-10 g: vvbzgmgggg", "7-12 p: jjjppjxpwrdp", "1-11 p: pnvpphxpppplppbxsppp", "8-9 s: ssrsssszrssm", "13-18 v: vvvvvvvvvvcvxvvvvvv", "3-11 l: llnllldlllllll", "5-6 q: sgwqqw", "8-9 z: zzzzzzzzz", "11-13 f: ffffffffffpfcffj", "1-2 g: hggkq", "2-3 m: xmjsmc", "12-13 r: rnghmlwvrzwfqdrrrrz", "3-4 w: wwwmfr", "6-16 t: tttttzttttttmttttt", "10-14 v: vvvvvvvvvtvvvhv", "2-4 m: wxnmrm", "3-5 x: xmwkp", "3-6 j: mnbdwgmvjmptj", "4-7 n: cgtdvsjnmrgk", "7-18 h: hchhhjflchhhtzhmvz", "4-7 g: gzxjgnggnggkc", "2-9 b: pdcbbgvwc", "3-13 s: ssksxsvslssjsssgss", "3-4 v: vvvdvf", "6-15 k: qkkkkkkkskkkkkkkbkkk", "6-7 h: hrwgqhh", "9-12 r: rcrwnrrrgrrtrrrz", "4-7 q: qqqqqqtq", "3-16 m: hqmqsjzskqnmcpfm", "1-15 v: vpbvvfvvvvjvvbv", "5-6 r: rrrrrjr", "5-7 n: nnnnnntpn", "4-5 d: dfddckzbdd", "8-9 f: fffffffzdf", "10-11 b: bbbbbbbbbgb", "1-4 d: xrqw", "8-16 q: qqqdqqqqqqqqqqqqqqq", "5-6 w: wfwwsr", "10-17 r: rrrrrrrrrhrrrrrrrr", "2-8 k: kqsflkkk", "3-10 f: fffpjffffpwd", "6-9 j: jvjjjjnvkjgj", "5-6 m: mmmhrrmm", "1-5 l: lnlllllwcpvb", "1-2 g: vcgg", "9-14 f: gspffffxltkfghzt", "16-17 l: llllllllllllllllll", "4-6 m: mmmvmnmm", "5-6 v: vvmvvk", "5-8 l: lfkmnkblnghwstb", "5-16 x: cktkxnckcpftdxtdbz", "4-5 b: bbbmbb", "8-9 w: wwwwwwwsw", "3-4 b: gwbd", "9-12 s: snssxssssssss", "7-10 v: vtdpvxvvvvgvvvctl", "11-17 c: dvfqpkmvzcwgvhcwcp", "3-4 s: sssr", "3-7 v: zplhwfvtvwv", "2-3 j: jjjj", "3-11 v: vvsvvvvvvvvvv", "9-10 h: hhhhhphhhh", "9-16 m: mkmcmmmmcmmmdcmmsmmr", "1-3 g: jgqgg", "4-18 s: ssswsssssssssssssrsc", "4-5 s: fsmgnhtlqbspcst", "4-5 k: nmthkhkzkbkdvrwhnk", "3-7 n: xptlnfnnnf", "8-11 t: ttttttgtttttt", "1-11 t: mtttttttkgtsttttlq", "3-8 h: xhhffqshhnzhg", "6-7 l: lllllgd", "4-9 d: cndddhfdzvn", "7-10 t: ttttwttzttkpttrdt", "4-14 m: mvsbxkwmcmnmrmjmv", "8-9 q: qjqqqhpqq", "4-6 t: ttttttt", "11-12 j: jjjtjjjjjjvjj", "4-5 t: lvlpv", "17-19 v: mvvvvvvvvvvvvvvvvxvv", "2-4 p: dpjkz", "4-5 z: zzzkzkz", "9-13 f: ffcffnfffdfhffff", "9-16 b: bbbbdbbbzbbbbbptbbqb", "15-18 j: jjjjjjjjjjjjjjzjbf", "2-8 p: qppnstfp", "4-7 d: pdgqdhj", "6-15 s: shrsssssstssjsbssvts", "11-12 r: rrrrrrrrrrrg", "2-12 b: jpgpmbvvfvlx", "10-12 k: skkkkkkkkkkkkhwk", "1-2 l: llllp", "3-8 c: cccccccc", "1-15 j: pjjjjjjjjjjjjjjj", "2-9 n: nnnnnnnnnnnnnn", "17-20 c: gqzgqvtxjqmqccwqcmcc", "14-15 z: zzzlvzqnzzzzzzz", "2-5 b: bbbbbb", "11-14 s: hsdpssvlzsxssm", "18-19 l: ldllxlrjflllvllllhl", "9-12 k: kkkvrjskkdfv", "10-15 n: wnnnlnnpnnnsnrnn", "1-3 h: hhdh", "10-12 p: ppprppppppppp", "1-3 h: hhhh", "10-11 w: wwrwcwwvwwh", "11-19 x: xdxcqxlrlvcxxdwdxbx", "9-10 s: sswssjrsssdswswspzrh", "10-11 n: nnnnnnnnnnn", "7-8 j: jjjjjnjjsjjjhv", "3-7 x: cnxxvxzcxxndx", "3-4 g: ggrg", "16-17 h: dvhhrqqrhkgbblgvh", "9-19 v: vxvvvvvtdmvvvvvvvvvv", "4-9 g: ggtjgxqsm", "6-8 c: cpxjcrcc", "11-19 w: wwwwnpwjwlwwwwwwwwww", "1-2 r: rrrr", "11-14 f: ffffffhvffbfnz", "8-10 t: tvpctqphvs", "8-12 l: flmpltlhldgblllmj", "9-10 n: nnnnnpnnpn", "7-12 b: bbblbblbbbbbbb", "3-7 l: lxlblrkrrld", "10-11 d: xhdddndtddw", "4-5 t: tttbtttt", "7-10 r: rrrrrrrrrrrrr", "6-15 w: rlnpwwtztjlwnswsjc", "10-11 c: ccccchcwcckmfglrfc", "3-4 t: szdd", "1-3 x: xckcj", "3-7 b: dzvqgbb", "2-3 c: dpft", "12-13 d: ddddddcddddxd", "8-10 h: cshhhhhhhhhr", "7-11 h: tlxhfzwjhhh", "2-15 j: crvqjnvgxmlljmj", "2-8 m: wmdhzmmmlmsmqmm", "7-8 z: zzzzzzfh", "3-4 c: cccr", "2-4 w: rwhw", "5-12 f: fvfqxkthvdmbftfncf", "8-19 j: rjjwhfzqjjprpsjgmvrq", "2-3 l: trlrkhgmz", "14-15 f: fwfffffffffnfmfff", "6-8 t: tqttttrwtttk", "1-6 v: vvvvwvvvvvwvvv", "3-6 d: lbbdvd", "11-18 q: blcqvfwprpqzznxpdq", "2-10 f: lffxfffqnzfqfbgfs", "13-15 j: jjjjjjjjjcjjqjfjj", "11-13 r: rrrhrlnjzslrrmrrw", "16-20 f: fhfffffffffffnffffff", "2-3 q: nqqpqq", "15-17 c: ccccccccccccccccc", "3-9 v: kjnfnvvvvvvvpvvvvg", "2-4 z: czzgxzbzzzznzzhzr", "6-10 p: pcgpkhcppsppjp", "2-3 g: mbgbfhtgd", "13-14 b: bkcbbbbbhbbrqvbt", "9-13 p: slsdztqphpppl", "6-8 b: bmbjmbbt", "11-19 q: bgxbqpfmclhfnqvfckqj", "5-8 h: jxhhqhqthhhgmhsrc", "6-7 d: bjdphddd", "1-2 x: txxxx", "5-8 h: hltghsnqrhq", "14-15 b: brlbqkcpsrppfbx", "4-12 n: rwsnnngpnnln", "4-6 w: wwwwwww", "1-6 v: svvlvb", "5-12 n: nnnndnnnnnnrtnnjn", "4-8 x: xxxhxxcz", "1-8 j: jjjjjjjmj", "1-5 m: mmpmr", "1-6 b: bknzjpcztb", "1-14 n: nnsmpdggcqsszrkxfxsx", "3-5 v: sdcgw", "6-12 x: wtxxnxldvfljxwl", "1-4 j: njzjjjtkjw", "3-10 g: ggrpgngggsgk", "2-7 g: jsbftgg", "10-11 z: bqznzxdlcnz", "5-6 r: rrrprr", "11-13 l: lttvqkjmtghlwllldl", "2-8 v: bvsvvvdv", "1-2 k: kkjkl", "18-19 c: cccccccccccccccccqc", "14-20 n: nvnlnnsnbtnrpzgnjbnm", "8-9 l: lxltrllzl", "3-4 l: llhzl", "4-5 z: zmnwk", "3-9 l: llmlllllqllllllsll", "6-11 n: fngcpdccnxr", "12-17 q: qqgqqqqzqqxqqqqqq", "6-7 n: nhntjzn", "7-8 w: wwwwwwww", "9-10 c: cccccccckx", "1-3 l: llprl", "5-6 x: xxwxxmxxrj", "12-17 b: bbbbbbbbbbbrbbbbb", "2-8 b: bbnnkssb", "2-12 z: rdcqnwcwnlpzz", "3-4 n: nnngnnn", "9-12 v: vvvvvlvvvvvv", "3-16 s: sbsvvzwjssssssss", "7-10 s: ssxshssbslsrssgs", "7-9 j: vnlhjcnjj", "10-13 f: fpffffffffffffffff", "14-19 v: vnvvvvcgvvvvchvvvvv", "7-8 c: vbcqtzccccwhv", "19-20 h: hhhhhhhhhhhvhhhhhhhr", "1-4 g: gmggg", "14-15 d: dddddddddddddnsd", "4-11 w: wjfwscxnlmvq", "2-10 h: khjfhnxdwh", "5-14 b: jpfblwbsmwgbqbbs", "5-6 m: mzmmbzmm", "2-8 p: pkspsqpppppprf", "3-4 n: qdnnmrnxjhwtwwjjdz", "9-14 p: pbrnpppfnpplbpppjpp", "4-5 r: rrrssk", "5-6 w: scwwdt", "7-11 z: fwlckzmzqrqt", "2-6 j: jjfjjhfjj", "1-6 c: cspcgczscbxgccc", "3-7 l: lllfldblcl", "5-18 f: ffffrfffffffffxffff", "18-19 j: jjjjjjjjgjjjjfjjjsjj", "11-12 f: fffnfffxkfzf", "3-6 h: hhhqfhvbjhhb", "1-3 l: gplzqfxlc", "6-13 t: tbsptltttnttt", "3-5 p: lpppd", "8-13 s: gksrfqdsmxdsssdr", "8-19 m: pmmphmrnmkmzmmhmmmmm", "3-4 t: ttmxt", "4-7 x: szxlhxxxdxdx", "4-10 t: tttttttttb", "2-5 p: pcpppp", "13-20 b: bbbbbbbbrzbbbbsbbdbb", "1-13 w: pwmwnwwwswwwwwwww", "5-6 t: tttttcttpttttttttttz", "12-13 t: tttttttttttjt", "6-7 j: swjxjjjjj", "5-17 g: gggggggggggtgggmgggp", "7-12 h: mkhthqhhhhhsghhhdhjw", "3-6 z: mndzrzs", "2-4 m: mfmh", "11-15 f: ffffckxfffffffgfmf", "4-10 w: spdwdlmjjw", "14-15 g: gggggggggggggggggg", "3-5 h: bmrfxqkvhrhbrdvx", "3-6 j: fjjgfjljjjjkjqjzj", "3-5 t: wfttbtt", "1-7 g: tgggggqggg", "10-11 c: ccccbccccccc", "1-2 w: pmww", "2-5 j: hjzwjrnjg", "3-6 x: cvxxcxmnt", "6-7 w: wwwwhvqw", "8-11 l: sbtflqzllrl", "12-13 f: fmfffffffflfvfj", "7-8 l: llllllbl", "3-6 b: bbbbbj", "7-14 t: dqmjszsmttjmttkstcv", "5-11 n: nnvnnbnnnhnnpv", "2-15 n: nfnnvnnnnnsnsnzp", "10-16 n: stfntnnnnnnnnnnnnnrn", "1-2 m: mcvmsrqjmmmm", "2-5 w: wwwwpwwhs", "6-8 b: lbcbctdb", "2-3 m: sdcrmxbmzhwmcqs", "3-5 t: gghtl", "15-17 b: wbzlbrfqnbbbbdzvb", "8-18 h: zzmvdtltdrpsnptmvwl", "5-7 t: ttttrtt", "6-7 g: gggggkg", "1-4 t: mtftdtk", "6-11 l: ljlllllsswlllll", "4-5 l: clllt", "2-10 c: dkccccccccccccccc", "16-19 h: hhhhhhhhhhhhhhhhhhh", "13-17 p: spppkvxpcpppppppm", "2-3 k: kgkzp", "3-4 j: jqvrsjscb", "2-3 q: nqqssfdlgqvhwmqdkr", "1-4 l: tllchllllll", "2-7 x: cxjdqxtjnhpc", "11-12 f: fffffffffffvf", "2-8 k: jnbkbtllhvpkk", "5-16 p: ppppppppppspppplpvpp", "8-10 x: xxxxxxxsxx", "7-11 h: hhhwhhhhlhchh", "5-7 d: ddddkcbpgm", "11-19 z: zzzzzzzkzzffzzzzzczz", "3-8 m: mmbmmmmmmt", "2-3 r: rrtbnzrr", "4-5 j: jjjjjjj", "3-9 f: ffkfffffffffff", "3-5 x: hxxhxxxp", "13-16 t: tgtjjtqksttgttrj", "6-14 v: vvvhvvvvvvvvhgvvhvc", "3-7 r: pwrnfrwz", "2-5 g: gngghg", "2-4 c: cccg", "6-7 t: fdttttcf", "10-15 f: fffsfrfffffffbffffff", "11-14 v: lvvvvvvlvvvxvr", "6-14 n: ntgnnnnvzqnlnkn", "5-6 q: qsdwgqq", "5-10 b: fbgbzbbpllbcbbsbb", "6-8 g: lfncdwsjggmgt", "5-13 f: fjffzfffffffhfff", "6-8 n: nxjnfnnl", "8-14 p: zfzkfgrpkmpxdhpthpb", "9-10 j: jjxmjjzmjjjdjjxjq", "8-18 l: glsllllllllllznllll", "5-6 f: sktcfsfflsbxfdc", "7-9 p: ppppcpppdp", "1-11 k: rkxckkxxjkvkkkkjkk", "6-8 g: gggvkgggg", "13-14 g: gggggggggggggzgg", "11-15 c: scczscccccccccc", "5-7 h: hhhhlhvhh", "7-10 k: kkkztkkvkbxknkrwk", "1-5 r: xmjlr", "16-17 p: ppppppppppppppppp", "12-15 f: ffffffffhffgffqf", "7-8 x: xsxxtxxxhxdxx", "3-7 m: tjznjgclmgmwxp", "1-10 x: mxxxxxxxxxx", "3-8 k: tzkwkkkfkkkzjgk", "3-5 n: nhnsn", "2-4 z: fzpzz", "1-3 n: szxnvwv", "2-6 c: ptlcqcc", "16-18 p: pqhsxrhrxpppzxpmnv", "2-11 l: bxlvktcvqzl", "4-9 f: fffvpfffffff", "2-7 k: kzkkkkkkkkk", "19-20 k: xklbjkxkkhpkzdnlwknd", "5-11 d: dbhvdlkzntkfsktptmtw", "3-4 m: mmmgmz", "5-11 z: mzzznxgxzzzzvzxkqs", "2-7 k: kkkkkkbkkk", "4-5 z: zzlzrdjz", "1-4 s: sssgsbsskssssssssss", "12-13 p: ppnpkpsppprxgcppppp", "5-7 m: tjrnsvqmbmmmhmmh", "5-8 j: sjjjrqjjjjjjj", "5-11 f: fffpflbflfvfhfm", "1-4 d: dddddd", "3-5 q: qqqqqq", "1-11 w: swwwjwwrjwg", "18-19 p: ppppppppppppppppplp", "16-17 c: cxcccvcccccccscnccc", "16-19 g: ggggggggggggggggggng", "7-9 j: jwjjzjjwhjjj", "13-15 k: kkrwfkkkkzkkkkkkrh", "1-11 r: xbghqccxlrr", "1-3 r: tqxppfrmrtm", "10-14 r: rprrrrrrrfrrrtrrrr", "12-18 x: xxxxxxxxqxxxxxxxxnx", "7-8 v: vvvxrvvjvwjsv", "7-10 d: ddddddgdlf", "13-14 l: llfdxhdqlclsll", "8-9 b: cbbbbbbbszjb", "2-5 k: kwkck", "17-18 p: mppfpshndcwjppjpwbk", "3-14 j: jjjjjjjjjjjcjjjjjjbj", "1-5 q: qqbzqq", "19-20 b: bbbbbbbbbbbbbbbbbbft", "16-17 b: tdvgkbqwftsbrqvbb", "5-13 p: pppzpptpgnclpppp", "4-5 s: mqsvs", "11-14 c: cccccccccncccc", "18-19 q: hbqkpvrnqjxmlqhzqqq", "5-10 r: lkrrrnmfbrrrps", "13-14 q: qqlqqqqsqqqqsz", "11-13 c: ccccccccccbcp", "1-3 g: ggggwgk", "1-13 r: rxrrxckcqrhvr", "3-7 f: fffffftfffhfff", "1-2 g: ggggzgggg", "4-11 x: xbbxldgjzxxjbcz", "2-6 c: bdgxpt", "11-12 m: mmmmmmmmmmmmm", "15-16 p: vwbbwpzzxwhtswpx", "6-7 x: xxxxkpnxxnw", "13-14 x: xxfxkpxjjfttjx", "7-11 f: rdbfffszfwffff", "5-7 d: gdtnkdj", "2-11 c: hcpchcchzccvcrr", "13-15 r: rrnrrrrrrrpdgrrvr", "6-7 h: hhhqfhh", "6-7 p: ppptppplp", "2-4 t: rwxttbpgwm", "6-12 b: tgxnbqtfvwgqxz", "2-5 s: sblqshv", "1-4 r: rfcrrmvkprw", "7-10 v: vhcvvvtvvqvvv", "12-17 t: zdtwcwtwtqtttttmtft", "5-9 v: vvvvvvvvvv", "4-11 h: fhhrschgjhhhd", "17-19 c: ccccccccccccccccccx", "5-9 w: lbjnrqpwwbjkd", "6-10 h: hqdqhchxwhshw", "6-7 c: cxcccccgcbc", "8-9 t: ttdttttttt", "11-12 n: wpnnvnnbnnlgnpnhnb", "5-15 r: rsrzrfgldrkvxqrrrqmj", "2-3 h: jlhhpj", "3-10 q: lrqwxqnmwq", "6-8 h: hhhhhghhg", "6-8 d: mdnddcdx", "8-14 w: wwwwwwwwwwwwwmwww", "9-14 q: bqkzzqzqmqtzqnqzq", "7-8 f: fffffvvf", "1-2 n: nfnnnnnnnnnn", "1-3 s: sqssnsbpcs", "2-11 h: rhbhhhrhhhdl", "4-5 p: vpfnp", "13-14 k: kkkkkkkkdkkktk", "7-11 n: znnffznhhzbckv", "1-3 d: xhldshlcq", "1-11 s: btstsgssgtxgp", "11-12 p: pppppppppppd", "3-8 k: kkkskkkk", "1-9 c: kfccjcccrccccch", "8-13 k: kkkkkkkqkkkkpkkkk", "2-10 l: cwllhjhbzlljsl", "3-11 t: tttttttttttt", "2-11 m: mmtlqbpbrmbh", "3-4 h: pwzhhm", "4-7 z: zzzzzczzv", "3-5 z: zgztzzz", "15-16 q: pqqqqqqqqrqqkqdq", "1-4 k: xkrkkpmgk", "2-6 p: cpqjdfsdpsd", "6-8 z: sxzzzfzzjs", "8-10 c: ccccmgcnclrccccc", "1-4 l: gllq", "18-19 z: zzwzzzzzzzzzzzzzvzw", "8-10 d: dddddddddddd", "1-5 j: jjjjjjtjjj", "9-11 k: ckkkkkkktxn", "1-9 b: bbbbbbbbbb", "13-15 m: mmmmmmwmmmmmmmv", "4-5 j: fmjsjjk", "4-10 q: clqbqqgxdq", "4-5 v: sqbvclwp", "3-5 n: qrjnn", "9-20 m: mlmhxmmmpmmmmmmmmmml", "6-7 r: rrrrrzhr", "11-13 b: bbbbbbbcbbbbtbbbbbb", "4-6 h: hhhhhhh", "1-3 q: qqqq", "6-10 h: hhhhhhhhzhhhk", "2-4 m: dwzh", "1-2 j: jjjjjj", "4-6 d: xdxqdnmddd", "9-10 g: ggggggggrgwg", "1-12 g: dgxggdrffgnf", "2-6 q: rlqqmqqckqqtfg", "5-11 t: cttttttclttktwtrtbw", "5-9 w: bvvbbwwsw", "6-9 h: hhxhhhhmxf", "5-6 k: kkkkkk", "1-5 w: wwwwn", "9-14 z: zgzzzzzqlnvgzl", "4-5 r: ssrrr", "6-7 b: sbkbbbbbcb", "4-7 x: xxxxxxxx", "8-18 k: kkpkpkkhkkkbmrkhkk", "5-13 w: mcnkvlsnmrrfwd", "10-20 k: xgknnkmkkkkqbxbkkzkk", "6-8 j: pmznnshjvb", "2-6 n: nnnnnnnn", "5-16 q: qqqqsqqqqqqqqqqtq", "3-4 l: llwf", "2-4 c: hcsz", "19-20 t: ttttttttttttttttttbt", "3-9 z: thdrpsxztzpqx", "1-4 p: pfzjfpp", "12-14 q: bqpnkqngjqqqqq", "1-7 d: zfldldd", "4-7 r: rwrlrjrrr", "5-13 m: spcgmdjgfmmjmhxmf", "2-3 j: jhjj", "1-10 v: vvpvvjdlvvhjvvvmvv", "3-5 r: fttrg", "1-5 z: zzzmz", "9-13 g: gggggggpgwgmh", "2-7 r: rrmkkzsqrfzhrfbp", "11-17 x: xxxxxxxxxxxxxxlxsxx", "4-6 d: rdddnd", "11-14 m: xkcddtwzfjbpmmc", "18-19 p: pppppppppppppppppfp", "1-12 q: qqqqqjqqqqqq", "9-12 x: xvxczcrmxqnzcxxs", "16-17 k: kkkkkkkkkkkkkkkkjdk", "4-8 c: cccnccccc", "5-7 s: srssssss", "19-20 p: ppppppppppppppppppnp", "2-3 v: vvvv", "1-13 j: jhlppkqxzdjgjdljhbvx", "6-7 w: wwwwvwxwbwk", "1-5 v: lvzvtrv", "1-2 t: wtwkk", "1-4 z: rzzxz", "3-4 c: cccfc", "4-11 j: qckjfcjkftjsjchbk", "2-10 d: wdjxdztbcdkn", "6-11 g: gghsggbgnnmlpb", "7-9 q: zwldqqngnqdmqztqfqqh", "5-6 w: wfqnzw", "1-4 d: vdddd", "1-3 g: ggbxzpng", "5-6 d: klfqdgfxfnlvhndtsx", "3-4 q: rhqqqrgqq", "11-12 p: ppppppppppppp", "12-16 m: lmblmhvsmmfmkfmlmh", "8-9 v: cpwrwvwqv", "2-4 l: prmvgmblzkzql", "1-2 w: wwwmwl", "9-10 c: zcccscccxc", "6-8 f: ftftvffr", "2-8 t: ttttttjqtttt", "11-13 t: sstlzdgttfthtqgnkdw", "4-5 k: vkfkx", "3-5 c: cccccccc", "2-10 d: nlgbhjdlxfcpc", "8-9 c: cchccccftcc", "5-6 t: tttcntttttntrtt", "3-15 n: dnncqnhnsnmqnvnpns", "4-5 p: phpxpp", "11-12 m: mmnmmfzmmmjfmm", "6-7 x: xxxxxxxx", "3-5 h: ndhphlhnx", "4-5 p: pppskc", "10-15 m: xmktnkwrmgvcmmhm", "4-6 r: rnrrrrrrr", "16-18 s: ssssssssssssssskss", "4-5 w: dphwqptwwqzwfr", "3-4 w: wwwl", "5-6 w: whfgwv", "2-6 d: sdhjddfcqr", "2-3 b: blhbbb", "3-4 t: rttt", "2-5 j: qwrfhjjmd", "1-2 m: rmmmmmmmmvmmm", "3-5 p: ppppppv", "8-15 p: pwpprmnvrpdpppp", "2-6 l: lwpnmxf", "11-13 l: jbhgtglpdjqjll", "2-4 p: ghqpl", "17-18 r: rrrrrrrrrrrrrrrrwrr", "5-8 r: ghwmrjrrrvkm", "1-3 l: llpl", "3-5 h: nhhqzphkh", "5-6 k: kkkgklk", "13-19 q: qqqqqqqqqqqqqqqqqqqq", "13-15 l: llllllllllllhlllll", "2-10 d: vdgwkdkwpdtnkrk", "6-12 r: mprbrgrqnlbrr", "7-8 f: ffffffgff", "2-6 w: wwvgdwqwwwwwft", "2-5 f: cfgwffdjqzf", "17-18 q: qqqqvqqqkqpqqnxqqq", "16-17 w: wwwwwwwwwwwwwhwkwwz", "2-16 t: zbxrtmhwtxdbnthq", "1-3 s: sjvtss", "9-10 h: hhhhhhhhmph", "7-8 t: ttljwvttnt", "9-10 f: ffffffffpgf", "7-11 q: lqgqxfzjqqqxdqmqq", "10-14 c: sccctgfcchghccccc", "6-8 f: xvcfhfvf", "3-5 h: hhhhzh", "3-6 p: pplppz", "2-5 j: jmhjjjjj", "2-10 v: kbvvvvvvrjvvvvvvvv", "9-15 g: gggggggggggfggggg", "14-15 z: zzxvfnczhqvdqqjfcjz", "7-12 v: vwvvjvxvrvmvvwvvvzmb", "3-5 h: fhhbh", "15-16 f: ffffffmffqfsffff", "1-5 j: psjjt", "6-11 c: vclcdcfcccnlj", "1-6 z: zkzzzz", "4-6 f: bngfff", "3-4 v: wpsv", "3-6 f: ffffjsff", "1-5 l: tlbxsxll", "5-13 m: jjmxmbmmjzssmcv", "1-2 g: qglglgl", "1-2 w: wwwx", "17-18 h: hshhhhhhhhhhhhhhbqhc", "13-14 s: bmptcjsfcplmcxrsgshs", "9-10 h: hhhhhhhhhh", "12-18 v: qvmvmtpjvvtzvbvvgvvw", "1-4 f: sffx", "1-8 w: lwwwwwwrw", "4-5 k: kkxxkn")
        List("2-7 p: pbhhzpmppb", "3-6 h: jkhnhwhx")
    }

    def countValidPasswordChallenge1(): Int = {
        val list: List[String]      = loadRessources()
        var count: Int              = 0
        var min: Int                = 0
        var max: Int                = 100000
        var char: Char              = ' '
        var password: String        = ""
        var decompose: Array[String] = Array()
        for (elementI <- list) {
            try {
                decompose = elementI.replace("-", " ").replace(":", "").split(" ")
                min = decompose(0).toInt
                max = decompose(1).toInt
                char = decompose(2).charAt(0)
                password = decompose(3)
                if (password.count(_ == char) >= min && password.count(_ == char) <= max) {
                    count += 1
                }
            } catch {
                case e: Exception => None
                    println("Exception")
            }
        }
        count
    }

    def countValidPasswordChallenge2(): Int = {
        val list: List[String]      = loadRessources()
        var count: Int              = 0
        var pos1: Int               = 0
        var pos2: Int               = 100000
        var char: Char              = ' '
        var password: String        = ""
        var decompose: Array[String] = Array()
        for (elementI <- list) {
            try {
                decompose = elementI.replace("-", " ").replace(":", "").split(" ")
                pos1 = decompose(0).toInt
                pos2 = decompose(1).toInt
                char = decompose(2).charAt(0)
                password = decompose(3)
                if (password(pos1 - 1) == char && password(pos2 - 1) != char
                  || password(pos1 - 1) != char && password(pos2 - 1) == char) {
                    count += 1
                }
            } catch {
                case e: Exception => None
                    println("Exception")
            }
        }
        count
    }
}
