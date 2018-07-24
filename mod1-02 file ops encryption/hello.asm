include C:\masm32\include\masm32rt.inc

.data
; (✿◠‿◠)  hex digits of pi from https://www.schneier.com/code/constants.txt
;         don't interrupt the sboxes and parray, they need to be declared in this exact order
sbox0 dd 0d1310ba6h, 098dfb5ach, 02ffd72dbh, 0d01adfb7h, 0b8e1afedh, 06a267e96h
sbox0q dd 0ba7c9045h, 0f12c7f99h, 024a19947h, 0b3916cf7h, 00801f2e2h, 0858efc16h
sbox0w dd 0636920d8h, 071574e69h, 0a458fea3h, 0f4933d7eh, 00d95748fh, 0728eb658h
sbox0e dd 0718bcd58h, 082154aeeh, 07b54a41dh, 0c25a59b5h, 09c30d539h, 02af26013h
sbox0r dd 0c5d1b023h, 0286085f0h, 0ca417918h, 0b8db38efh, 08e79dcb0h, 0603a180eh
sbox0t dd 06c9e0e8bh, 0b01e8a3eh, 0d71577c1h, 0bd314b27h, 078af2fdah, 055605c60h
sbox0y dd 0e65525f3h, 0aa55ab94h, 057489862h, 063e81440h, 055ca396ah, 02aab10b6h
sbox0u dd 0b4cc5c34h, 01141e8ceh, 0a15486afh, 07c72e993h, 0b3ee1411h, 0636fbc2ah
sbox0i dd 02ba9c55dh, 0741831f6h, 0ce5c3e16h, 09b87931eh, 0afd6ba33h, 06c24cf5ch
sbox0o dd 07a325381h, 028958677h, 03b8f4898h, 06b4bb9afh, 0c4bfe81bh, 066282193h
sbox0p dd 061d809cch, 0fb21a991h, 0487cac60h, 05dec8032h, 0ef845d5dh, 0e98575b1h
sbox0a dd 0dc262302h, 0eb651b88h, 023893e81h, 0d396acc5h, 00f6d6ff3h, 083f44239h
sbox0s dd 02e0b4482h, 0a4842004h, 069c8f04ah, 09e1f9b5eh, 021c66842h, 0f6e96c9ah
sbox0d dd 0670c9c61h, 0abd388f0h, 06a51a0d2h, 0d8542f68h, 0960fa728h, 0ab5133a3h
sbox0f dd 06eef0b6ch, 0137a3be4h, 0ba3bf050h, 07efb2a98h, 0a1f1651dh, 039af0176h
sbox0g dd 066ca593eh, 082430e88h, 08cee8619h, 0456f9fb4h, 07d84a5c3h, 03b8b5ebeh
sbox0h dd 0e06f75d8h, 085c12073h, 0401a449fh, 056c16aa6h, 04ed3aa62h, 0363f7706h
sbox0j dd 01bfedf72h, 0429b023dh, 037d0d724h, 0d00a1248h, 0db0fead3h, 049f1c09bh
sbox0k dd 0075372c9h, 080991b7bh, 025d479d8h, 0f6e8def7h, 0e3fe501ah, 0b6794c3bh
sbox0l dd 0976ce0bdh, 004c006bah, 0c1a94fb6h, 0409f60c4h, 05e5c9ec2h, 0196a2463h
sbox0z dd 068fb6fafh, 03e6c53b5h, 01339b2ebh, 03b52ec6fh, 06dfc511fh, 09b30952ch
sbox0x dd 0cc814544h, 0af5ebd09h, 0bee3d004h, 0de334afdh, 0660f2807h, 0192e4bb3h
sbox0c dd 0c0cba857h, 045c8740fh, 0d20b5f39h, 0b9d3fbdbh, 05579c0bdh, 01a60320ah
sbox0v dd 0d6a100c6h, 0402c7279h, 0679f25feh, 0fb1fa3cch, 08ea5e9f8h, 0db3222f8h
sbox0b dd 03c7516dfh, 0fd616b15h, 02f501ec8h, 0ad0552abh, 0323db5fah, 0fd238760h
sbox0n dd 053317b48h, 03e00df82h, 09e5c57bbh, 0ca6f8ca0h, 01a87562eh, 0df1769dbh
sbox0m dd 0d542a8f6h, 0287effc3h, 0ac6732c6h, 08c4f5573h, 0695b27b0h, 0bbca58c8h
sbox0aq dd 0e1ffa35dh, 0b8f011a0h, 010fa3d98h, 0fd2183b8h, 04afcb56ch, 02dd1d35bh
sbox0aw dd 09a53e479h, 0b6f84565h, 0d28e49bch, 04bfb9790h, 0e1ddf2dah, 0a4cb7e33h
sbox0ae dd 062fb1341h, 0cee4c6e8h, 0ef20cadah, 036774c01h, 0d07e9efeh, 02bf11fb4h
sbox0ar dd 095dbda4dh, 0ae909198h, 0eaad8e71h, 06b93d5a0h, 0d08ed1d0h, 0afc725e0h
sbox0at dd 08e3c5b2fh, 08e7594b7h, 08ff6e2fbh, 0f2122b64h, 08888b812h, 0900df01ch
sbox0ay dd 04fad5ea0h, 0688fc31ch, 0d1cff191h, 0b3a8c1adh, 02f2f2218h, 0be0e1777h
sbox0au dd 0ea752dfeh, 08b021fa1h, 0e5a0cc0fh, 0b56f74e8h, 018acf3d6h, 0ce89e299h
sbox0ai dd 0b4a84fe0h, 0fd13e0b7h, 07cc43b81h, 0d2ada8d9h, 0165fa266h, 080957705h
sbox0ao dd 093cc7314h, 0211a1477h, 0e6ad2065h, 077b5fa86h, 0c75442f5h, 0fb9d35cfh
sbox0ap dd 0ebcdaf0ch, 07b3e89a0h, 0d6411bd3h, 0ae1e7e49h, 000250e2dh, 02071b35eh
sbox0aa dd 0226800bbh, 057b8e0afh, 02464369bh, 0f009b91eh, 05563911dh, 059dfa6aah
sbox0as dd 078c14389h, 0d95a537fh, 0207d5ba2h, 002e5b9c5h, 083260376h, 06295cfa9h
sbox0ad dd 011c81968h, 04e734a41h, 0b3472dcah, 07b14a94ah, 01b510052h, 09a532915h
sbox0af dd 0d60f573fh, 0bc9bc6e4h, 02b60a476h, 081e67400h, 008ba6fb5h, 0571be91fh
sbox0ag dd 0f296ec6bh, 02a0dd915h, 0b6636521h, 0e7b9f9b6h, 0ff34052eh, 0c5855664h
sbox0ah dd 053b02d5dh, 0a99f8fa1h, 008ba4799h, 06e85076ah
sbox1 dd 04b7a70e9h, 0b5b32944h, 0db75092eh, 0c4192623h, 0ad6ea6b0h, 049a7df7dh
sbox1q dd 09cee60b8h, 08fedb266h, 0ecaa8c71h, 0699a17ffh, 05664526ch, 0c2b19ee1h
sbox1w dd 0193602a5h, 075094c29h, 0a0591340h, 0e4183a3eh, 03f54989ah, 05b429d65h
sbox1e dd 06b8fe4d6h, 099f73fd6h, 0a1d29c07h, 0efe830f5h, 04d2d38e6h, 0f0255dc1h
sbox1r dd 04cdd2086h, 08470eb26h, 06382e9c6h, 0021ecc5eh, 009686b3fh, 03ebaefc9h
sbox1t dd 03c971814h, 06b6a70a1h, 0687f3584h, 052a0e286h, 0b79c5305h, 0aa500737h
sbox1y dd 03e07841ch, 07fdeae5ch, 08e7d44ech, 05716f2b8h, 0b03ada37h, 0f0500c0dh
sbox1u dd 0f01c1f04h, 00200b3ffh, 0ae0cf51ah, 03cb574b2h, 025837a58h, 0dc0921bdh
sbox1i dd 0d19113f9h, 07ca92ff6h, 094324773h, 022f54701h, 03ae5e581h, 037c2dadch
sbox1o dd 0c8b57634h, 09af3dda7h, 0a9446146h, 00fd0030eh, 0ecc8c73eh, 0a4751e41h
sbox1p dd 0e238cd99h, 03bea0e2fh, 03280bba1h, 0183eb331h, 04e548b38h, 04f6db908h
sbox1a dd 06f420d03h, 0f60a04bfh, 02cb81290h, 024977c79h, 05679b072h, 0bcaf89afh
sbox1s dd 0de9a771fh, 0d9930810h, 0b38bae12h, 0dccf3f2eh, 05512721fh, 02e6b7124h
sbox1d dd 0501adde6h, 09f84cd87h, 07a584718h, 07408da17h, 0bc9f9abch, 0e94b7d8ch
sbox1f dd 0ec7aec3ah, 0db851dfah, 063094366h, 0c464c3d2h, 0ef1c1847h, 03215d908h
sbox1g dd 0dd433b37h, 024c2ba16h, 012a14d43h, 02a65c451h, 050940002h, 0133ae4ddh
sbox1h dd 071dff89eh, 010314e55h, 081ac77d6h, 05f11199bh, 0043556f1h, 0d7a3c76bh
sbox1j dd 03c11183bh, 05924a509h, 0f28fe6edh, 097f1fbfah, 09ebabf2ch, 01e153c6eh
sbox1k dd 086e34570h, 0eae96fb1h, 0860e5e0ah, 05a3e2ab3h, 0771fe71ch, 04e3d06fah
sbox1l dd 02965dcb9h, 099e71d0fh, 0803e89d6h, 05266c825h, 02e4cc978h, 09c10b36ah
sbox1z dd 0c6150ebah, 094e2ea78h, 0a5fc3c53h, 01e0a2df4h, 0f2f74ea7h, 0361d2b3dh
sbox1x dd 01939260fh, 019c27960h, 05223a708h, 0f71312b6h, 0ebadfe6eh, 0eac31f66h
sbox1c dd 0e3bc4595h, 0a67bc883h, 0b17f37d1h, 0018cff28h, 0c332ddefh, 0be6c5aa5h
sbox1v dd 065582185h, 068ab9802h, 0eecea50fh, 0db2f953bh, 02aef7dadh, 05b6e2f84h
sbox1b dd 01521b628h, 029076170h, 0ecdd4775h, 0619f1510h, 013cca830h, 0eb61bd96h
sbox1n dd 00334fe1eh, 0aa0363cfh, 0b5735c90h, 04c70a239h, 0d59e9e0bh, 0cbaade14h
sbox1m dd 0eecc86bch, 060622ca7h, 09cab5cabh, 0b2f3846eh, 0648b1eafh, 019bdf0cah
sbox1aq dd 0a02369b9h, 0655abb50h, 040685a32h, 03c2ab4b3h, 0319ee9d5h, 0c021b8f7h
sbox1aw dd 09b540b19h, 0875fa099h, 095f7997eh, 0623d7da8h, 0f837889ah, 097e32d77h
sbox1ae dd 011ed935fh, 016681281h, 00e358829h, 0c7e61fd6h, 096dedfa1h, 07858ba99h
sbox1ar dd 057f584a5h, 01b227263h, 09b83c3ffh, 01ac24696h, 0cdb30aebh, 0532e3054h
sbox1at dd 08fd948e4h, 06dbc3128h, 058ebf2efh, 034c6ffeah, 0fe28ed61h, 0ee7c3c73h
sbox1ay dd 05d4a14d9h, 0e864b7e3h, 042105d14h, 0203e13e0h, 045eee2b6h, 0a3aaabeah
sbox1au dd 0db6c4f15h, 0facb4fd0h, 0c742f442h, 0ef6abbb5h, 0654f3b1dh, 041cd2105h
sbox1ai dd 0d81e799eh, 086854dc7h, 0e44b476ah, 03d816250h, 0cf62a1f2h, 05b8d2646h
sbox1ao dd 0fc8883a0h, 0c1c7b6a3h, 07f1524c3h, 069cb7492h, 047848a0bh, 05692b285h
sbox1ap dd 0095bbf00h, 0ad19489dh, 01462b174h, 023820e00h, 058428d2ah, 00c55f5eah
sbox1aa dd 01dadf43eh, 0233f7061h, 03372f092h, 08d937e41h, 0d65fecf1h, 06c223bdbh
sbox1as dd 07cde3759h, 0cbee7460h, 04085f2a7h, 0ce77326eh, 0a6078084h, 019f8509eh
sbox1ad dd 0e8efd855h, 061d99735h, 0a969a7aah, 0c50c06c2h, 05a04abfch, 0800bcadch
sbox1af dd 09e447a2eh, 0c3453484h, 0fdd56705h, 00e1e9ec9h, 0db73dbd3h, 0105588cdh
sbox1ag dd 0675fda79h, 0e3674340h, 0c5c43465h, 0713e38d8h, 03d28f89eh, 0f16dff20h
sbox1ah dd 0153e21e7h, 08fb03d4ah, 0e6e39f2bh, 0db83adf7h
sbox2 dd 0e93d5a68h, 0948140f7h, 0f64c261ch, 094692934h, 0411520f7h, 07602d4f7h
sbox2q dd 0bcf46b2eh, 0d4a20068h, 0d4082471h, 03320f46ah, 043b7d4b7h, 0500061afh
sbox2w dd 01e39f62eh, 097244546h, 014214f74h, 0bf8b8840h, 04d95fc1dh, 096b591afh
sbox2e dd 070f4ddd3h, 066a02f45h, 0bfbc09ech, 003bd9785h, 07fac6dd0h, 031cb8504h
sbox2r dd 096eb27b3h, 055fd3941h, 0da2547e6h, 0abca0a9ah, 028507825h, 0530429f4h
sbox2t dd 00a2c86dah, 0e9b66dfbh, 068dc1462h, 0d7486900h, 0680ec0a4h, 027a18deeh
sbox2y dd 04f3ffea2h, 0e887ad8ch, 0b58ce006h, 07af4d6b6h, 0aace1e7ch, 0d3375fech
sbox2u dd 0ce78a399h, 0406b2a42h, 020fe9e35h, 0d9f385b9h, 0ee39d7abh, 03b124e8bh
sbox2i dd 01dc9faf7h, 04b6d1856h, 026a36631h, 0eae397b2h, 03a6efa74h, 0dd5b4332h
sbox2o dd 06841e7f7h, 0ca7820fbh, 0fb0af54eh, 0d8feb397h, 0454056ach, 0ba489527h
sbox2p dd 055533a3ah, 020838d87h, 0fe6ba9b7h, 0d096954bh, 055a867bch, 0a1159a58h
sbox2a dd 0cca92963h, 099e1db33h, 0a62a4a56h, 03f3125f9h, 05ef47e1ch, 09029317ch
sbox2s dd 0fdf8e802h, 004272f70h, 080bb155ch, 005282ce3h, 095c11548h, 0e4c66d22h
sbox2d dd 048c1133fh, 0c70f86dch, 007f9c9eeh, 041041f0fh, 0404779a4h, 05d886e17h
sbox2f dd 0325f51ebh, 0d59bc0d1h, 0f2bcc18fh, 041113564h, 0257b7834h, 0602a9c60h
sbox2g dd 0dff8e8a3h, 01f636c1bh, 00e12b4c2h, 002e1329eh, 0af664fd1h, 0cad18115h
sbox2h dd 06b2395e0h, 0333e92e1h, 03b240b62h, 0eebeb922h, 085b2a20eh, 0e6ba0d99h
sbox2j dd 0de720c8ch, 02da2f728h, 0d0127845h, 095b794fdh, 0647d0862h, 0e7ccf5f0h
sbox2k dd 05449a36fh, 0877d48fah, 0c39dfd27h, 0f33e8d1eh, 00a476341h, 0992eff74h
sbox2l dd 03a6f6eabh, 0f4f8fd37h, 0a812dc60h, 0a1ebddf8h, 0991be14ch, 0db6e6b0dh
sbox2z dd 0c67b5510h, 06d672c37h, 02765d43bh, 0dcd0e804h, 0f1290dc7h, 0cc00ffa3h
sbox2x dd 0b5390f92h, 0690fed0bh, 0667b9ffbh, 0cedb7d9ch, 0a091cf0bh, 0d9155ea3h
sbox2c dd 0bb132f88h, 0515bad24h, 07b9479bfh, 0763bd6ebh, 037392eb3h, 0cc115979h
sbox2v dd 08026e297h, 0f42e312dh, 06842ada7h, 0c66a2b3bh, 012754ccch, 0782ef11ch
sbox2b dd 06a124237h, 0b79251e7h, 006a1bbe6h, 04bfb6350h, 01a6b1018h, 011caedfah
sbox2n dd 03d25bdd8h, 0e2e1c3c9h, 044421659h, 00a121386h, 0d90cec6eh, 0d5abea2ah
sbox2m dd 064af674eh, 0da86a85fh, 0bebfe988h, 064e4c3feh, 09dbc8057h, 0f0f7c086h
sbox2aq dd 060787bf8h, 06003604dh, 0d1fd8346h, 0f6381fb0h, 07745ae04h, 0d736fccch
sbox2aw dd 083426b33h, 0f01eab71h, 0b0804187h, 03c005e5fh, 077a057beh, 0bde8ae24h
sbox2ae dd 055464299h, 0bf582e61h, 04e58f48fh, 0f2ddfda2h, 0f474ef38h, 08789bdc2h
sbox2ar dd 05366f9c3h, 0c8b38e74h, 0b475f255h, 046fcd9b9h, 07aeb2661h, 08b1ddf84h
sbox2at dd 0846a0e79h, 0915f95e2h, 0466e598eh, 020b45770h, 08cd55591h, 0c902de4ch
sbox2ay dd 0b90bace1h, 0bb8205d0h, 011a86248h, 07574a99eh, 0b77f19b6h, 0e0a9dc09h
sbox2au dd 0662d09a1h, 0c4324633h, 0e85a1f02h, 009f0be8ch, 04a99a025h, 01d6efe10h
sbox2ai dd 01ab93d1dh, 00ba5a4dfh, 0a186f20fh, 02868f169h, 0dcb7da83h, 0573906feh
sbox2ao dd 0a1e2ce9bh, 04fcd7f52h, 050115e01h, 0a70683fah, 0a002b5c4h, 00de6d027h
sbox2ap dd 09af88c27h, 0773f8641h, 0c3604c06h, 061a806b5h, 0f0177a28h, 0c0f586e0h
sbox2aa dd 0006058aah, 030dc7d62h, 011e69ed7h, 02338ea63h, 053c2dd94h, 0c2c21634h
sbox2as dd 0bbcbee56h, 090bcb6deh, 0ebfc7da1h, 0ce591d76h, 06f05e409h, 04b7c0188h
sbox2ad dd 039720a3dh, 07c927c24h, 086e3725fh, 0724d9db9h, 01ac15bb4h, 0d39eb8fch
sbox2af dd 0ed545578h, 008fca5b5h, 0d83d7cd3h, 04dad0fc4h, 01e50ef5eh, 0b161e6f8h
sbox2ag dd 0a28514d9h, 06c51133ch, 06fd5c7e7h, 056e14ec4h, 0362abfceh, 0ddc6c837h
sbox2ah dd 0d79a3234h, 092638212h, 0670efa8eh, 0406000e0h
sbox3 dd 03a39ce37h, 0d3faf5cfh, 0abc27737h, 05ac52d1bh, 05cb0679eh, 04fa33742h
sbox3q dd 0d3822740h, 099bc9bbeh, 0d5118e9dh, 0bf0f7315h, 0d62d1c7eh, 0c700c47bh
sbox3w dd 0b78c1b6bh, 021a19045h, 0b26eb1beh, 06a366eb4h, 05748ab2fh, 0bc946e79h
sbox3e dd 0c6a376d2h, 06549c2c8h, 0530ff8eeh, 0468dde7dh, 0d5730a1dh, 04cd04dc6h
sbox3r dd 02939bbdbh, 0a9ba4650h, 0ac9526e8h, 0be5ee304h, 0a1fad5f0h, 06a2d519ah
sbox3t dd 063ef8ce2h, 09a86ee22h, 0c089c2b8h, 043242ef6h, 0a51e03aah, 09cf2d0a4h
sbox3y dd 083c061bah, 09be96a4dh, 08fe51550h, 0ba645bd6h, 02826a2f9h, 0a73a3ae1h
sbox3u dd 04ba99586h, 0ef5562e9h, 0c72fefd3h, 0f752f7dah, 03f046f69h, 077fa0a59h
sbox3i dd 080e4a915h, 087b08601h, 09b09e6adh, 03b3ee593h, 0e990fd5ah, 09e34d797h
sbox3o dd 02cf0b7d9h, 0022b8b51h, 096d5ac3ah, 0017da67dh, 0d1cf3ed6h, 07c7d2d28h
sbox3p dd 01f9f25cfh, 0adf2b89bh, 05ad6b472h, 05a88f54ch, 0e029ac71h, 0e019a5e6h
sbox3a dd 047b0acfdh, 0ed93fa9bh, 0e8d3c48dh, 0283b57cch, 0f8d56629h, 079132e28h
sbox3s dd 0785f0191h, 0ed756055h, 0f7960e44h, 0e3d35e8ch, 015056dd4h, 088f46dbah
sbox3d dd 003a16125h, 00564f0bdh, 0c3eb9e15h, 03c9057a2h, 097271aech, 0a93a072ah
sbox3f dd 01b3f6d9bh, 01e6321f5h, 0f59c66fbh, 026dcf319h, 07533d928h, 0b155fdf5h
sbox3g dd 003563482h, 08aba3cbbh, 028517711h, 0c20ad9f8h, 0abcc5167h, 0ccad925fh
sbox3h dd 04de81751h, 03830dc8eh, 0379d5862h, 09320f991h, 0ea7a90c2h, 0fb3e7bceh
sbox3j dd 05121ce64h, 0774fbe32h, 0a8b6e37eh, 0c3293d46h, 048de5369h, 06413e680h
sbox3k dd 0a2ae0810h, 0dd6db224h, 069852dfdh, 009072166h, 0b39a460ah, 06445c0ddh
sbox3l dd 0586cdecfh, 01c20c8aeh, 05bbef7ddh, 01b588d40h, 0ccd2017fh, 06bb4e3bbh
sbox3z dd 0dda26a7eh, 03a59ff45h, 03e350a44h, 0bcb4cdd5h, 072eacea8h, 0fa6484bbh
sbox3x dd 08d6612aeh, 0bf3c6f47h, 0d29be463h, 0542f5d9eh, 0aec2771bh, 0f64e6370h
sbox3c dd 0740e0d8dh, 0e75b1357h, 0f8721671h, 0af537d5dh, 04040cb08h, 04eb4e2cch
sbox3v dd 034d2466ah, 00115af84h, 0e1b00428h, 095983a1dh, 006b89fb4h, 0ce6ea048h
sbox3b dd 06f3f3b82h, 03520ab82h, 0011a1d4bh, 0277227f8h, 0611560b1h, 0e7933fdch
sbox3n dd 0bb3a792bh, 0344525bdh, 0a08839e1h, 051ce794bh, 02f32c9b7h, 0a01fbac9h
sbox3m dd 0e01cc87eh, 0bcc7d1f6h, 0cf0111c3h, 0a1e8aac7h, 01a908749h, 0d44fbd9ah
sbox3aq dd 0d0dadecbh, 0d50ada38h, 00339c32ah, 0c6913667h, 08df9317ch, 0e0b12b4fh
sbox3aw dd 0f79e59b7h, 043f5bb3ah, 0f2d519ffh, 027d9459ch, 0bf97222ch, 015e6fc2ah
sbox3ae dd 00f91fc71h, 09b941525h, 0fae59361h, 0ceb69cebh, 0c2a86459h, 012baa8d1h
sbox3ar dd 0b6c1075eh, 0e3056a0ch, 010d25065h, 0cb03a442h, 0e0ec6e0eh, 01698db3bh
sbox3at dd 04c98a0beh, 03278e964h, 09f1f9532h, 0e0d392dfh, 0d3a0342bh, 08971f21eh
sbox3ay dd 01b0a7441h, 04ba3348ch, 0c5be7120h, 0c37632d8h, 0df359f8dh, 09b992f2eh
sbox3au dd 0e60b6f47h, 00fe3f11dh, 0e54cda54h, 01edad891h, 0ce6279cfh, 0cd3e7e6fh
sbox3ai dd 01618b166h, 0fd2c1d05h, 0848fd2c5h, 0f6fb2299h, 0f523f357h, 0a6327623h
sbox3ao dd 093a83531h, 056cccd02h, 0acf08162h, 05a75ebb5h, 06e163697h, 088d273cch
sbox3ap dd 0de966292h, 081b949d0h, 04c50901bh, 071c65614h, 0e6c6c7bdh, 0327a140ah
sbox3aa dd 045e1d006h, 0c3f27b9ah, 0c9aa53fdh, 062a80f00h, 0bb25bfe2h, 035bdd2f6h
sbox3as dd 071126905h, 0b2040222h, 0b6cbcf7ch, 0cd769c2bh, 053113ec0h, 01640e3d3h
sbox3ad dd 038abbd60h, 02547adf0h, 0ba38209ch, 0f746ce76h, 077afa1c5h, 020756060h
sbox3af dd 085cbfe4eh, 08ae88dd8h, 07aaaf9b0h, 04cf9aa7eh, 01948c25ch, 002fb8a8ch
sbox3ag dd 001c36ae4h, 0d6ebe1f9h, 090d4f869h, 0a65cdea0h, 03f09252dh, 0c208e69fh
sbox3ah dd 0b74e6132h, 0ce77e25bh, 0578fdfe3h, 03ac372e6h
parray dd 0243f6a88h, 0085a308d3h, 0013198a2eh, 0003707344h, 00a4093822h, 00299f31d0h, 00082efa98h, 00ec4e6c89h, 00452821e6h, 0038d01377h, 00be5466cfh, 0034e90c6ch, 00c0ac29b7h, 00c97c50ddh, 003f84d5b5h, 00b5470917h, 009216d5d9h, 008979fb1bh
; (✿◠‿◠) It is very important that encodeme is declared after parray because the key initialization loop ends with a pointer comparison (its contents do not matter)
encodeme db "The last question was asked for the first time, half in jest, on May 21, 2061, at a time when humanity first stepped into the light. The question came about as a result of a five dollar bet over highballs, and it happened this way:",0
; (✿◠‿◠) key length is 4 to 56 bytes, doubled to make rollover easier
key db "let there be light"
key2 db "let there be light"

	currentdirectory db 260 DUP(0) ; (✿◠‿◠)  :::WARNING::: UNIDENTIFIED BEHAVIOR IF FILE PATH EXCEEDS MAX_PATH
	currentmaskTXT db 260 DUP (0)
	currentmaskALL db 260 DUP (0)
	filetowork db 260 DUP(0)
	newfilename db 260 DUP(0)
	filebuffer db 9 DUP (0)
	crlf db 0dh, 0ah, 0

	arg1 db 128 DUP(0)
	arg2 db 128 DUP(0)
	arg3 db 128 DUP(0)
	ArgErrorMsg db "Error with arguments.",0
	action db 0 ; (✿◠‿◠) 1 for encryption, 0 for decryption
	
	gonenc db "gonna encrypt ",0
	gondec db "gonna decrypt ",0
	
.data?
	foundfile WIN32_FIND_DATA<>
	writtenbytes dd ?
	readbytes dd ?
	filehandle dd ?

	
.code

;----------------------------------->
;   Begin Blowfish cipher code     <
;----------------------------------->

fbox proc ; (✿◠‿◠) runs eax through the f-box while preserving ebx,ecx, and edx
push edx
push ebx
push ecx

mov ebx, 0ffh ; (✿◠‿◠) d
and ebx, eax
push ebx

mov ebx, 0ff00h ; (✿◠‿◠) c
and ebx, eax
shr ebx, 8
push ebx

mov ebx, 0ff0000h ; (✿◠‿◠) b
and ebx, eax
shr ebx, 16
push ebx

mov ebx, 0ff000000h ; (✿◠‿◠) a
and ebx, eax
shr ebx, 24
push ebx

pop ebx
imul ebx, 4
mov eax, [sbox0 + ebx]

pop ebx
imul ebx, 4
add eax, [sbox1 + ebx]

pop ebx
imul ebx, 4
xor eax, [sbox2 + ebx]

pop ebx
imul ebx, 4
add eax, [sbox3 + ebx]

pop ecx
pop ebx
pop edx

ret

fbox endp

blowfish_encrypt proc ; (✿◠‿◠) left is eax, right is ebx
	
	lea ecx, [parray]
	add ecx, 8 ; (✿◠‿◠) this is a hack for stopping 8 bytes (2 dds) from end
	keepgoing:
		sub ecx, 8
		xor eax, [ecx]
		push eax
		push ecx
		call fbox
		pop ecx
		xor eax, ebx
		pop ebx
		add ecx, 12
		lea edx, [encodeme]
		cmp ecx, edx
			jl keepgoing
	
	push eax
	mov eax, ebx
	pop ebx
	
	sub ecx, 8
	xor ebx, [ecx]
	add ecx, 4
	xor eax, [ecx]
	ret

blowfish_encrypt endp

blowfish_decrypt proc ; (✿◠‿◠) left is eax, right is ebx
	
	lea ecx, [encodeme]
	sub ecx, 4

	sub ecx, 4 ; (✿◠‿◠) this is a hack for stopping 4 bytes (1 dd) from end
	keepgoing:
		add ecx, 4
		xor eax, [ecx]
		push eax
		push ecx
		call fbox
		pop ecx
		xor eax, ebx
		pop ebx
		
		sub ecx, 8
		lea edx, [parray]
		cmp ecx, edx
			jg keepgoing
	
	push eax
	mov eax, ebx
	pop ebx
	
	add ecx, 4
	xor ebx, [ecx]
	sub ecx, 4
	xor eax, [ecx]
	ret

blowfish_decrypt endp

initialize_blowfish proc

	; (✿◠‿◠) initialize key into P array and S-boxes
	lea ecx, [parray]
	lea ebx, [key]
	noadjust:
	mov eax, [ebx]
	xor [ecx], eax

	add ebx, 4
	add ecx, 4
	lea edx, [encodeme]
	cmp ecx, edx
	je donepkey
	lea edx, [key2]
	cmp ebx, edx
	jl noadjust
	sub ebx, edx
	lea ebx, [key+ebx]
	jmp noadjust
	donepkey:

	; (✿◠‿◠)  key generation begins with 0 input to recreate P array
	xor eax,eax
	xor ebx,ebx

	lea ecx, [parray]

	keepgoingparray:
		push ecx
		call blowfish_encrypt
		pop ecx

		mov [ecx], eax
		add ecx, 4
		mov [ecx], ebx
		add ecx, 4

		lea edx, [encodeme]
		cmp ecx, edx
			jl keepgoingparray

	lea ecx, [sbox0]

	keepgoingsbox:
		push ecx
		call blowfish_encrypt
		pop ecx

		mov [ecx], eax
		add ecx, 4

		mov [ecx], ebx
		add ecx, 4
		lea edx, [parray]
		cmp ecx, edx
		jl keepgoingsbox

	ret

initialize_blowfish endp

;----------------------------------->
;     End Blowfish cipher code     <
;----------------------------------->

;----------------------------------->
;   Begin File Operations Code     <
;----------------------------------->

loadpath proc ;writes relative path (currentdirectory) and filename into filetowork string
	lea edx, [filetowork] ;blanks filetowork
	blankingFILE:
	mov al, [edx]
	cmp al, 0
	je doneblankingFILE
	mov BYTE PTR[edx], 0
	inc edx
	jmp blankingFILE
	doneblankingFILE:
	
	lea ecx,[currentdirectory]
	
	lea edx, [filetowork] ;copies currentdirectory into filetowork
	copyingFILE:
	mov al, [ecx]
	mov BYTE PTR [edx], al
	cmp al, 0
	je donecopyingFILE
	inc ecx
	inc edx
	jmp copyingFILE
	donecopyingFILE:
	mov BYTE PTR [edx], "\"
	inc edx
	
	lea ecx, [foundfile.cFileName] ;appends filename to filetowork
	appendingFILE:
	mov al, [ecx]
	mov BYTE PTR [edx], al
	cmp al, 0
	je doneappendingFILE
	inc ecx
	inc edx
	jmp appendingFILE
	doneappendingFILE:
	ret
loadpath endp

loadnewfile proc ;copies filetowork to newfilename
	lea edx, [newfilename] ;blanks newfilename
	blankingFILE:
	mov al, [edx]
	cmp al, 0
	je doneblankingFILE
	mov BYTE PTR[edx], 0
	inc edx
	jmp blankingFILE
	doneblankingFILE:
	
	lea ecx,[filetowork]
	
	lea edx, [newfilename] ;copies filetowork into newfilename
	copyingFILE:
	mov al, [ecx]
	mov BYTE PTR [edx], al
	cmp al, 0
	je donecopyingFILE
	inc ecx
	inc edx
	jmp copyingFILE
	donecopyingFILE:
	;mov BYTE PTR [edx], "\"
	;inc edx
	
	ret
loadnewfile endp

dofile proc ; works on file

;if encrypted and action is 1: skip
;if encrypted and action is 0: decrypt
;if not encrypted and action is 1: encrypt
;if not encrypted and action is 0: skip

	lea eax, [filetowork]
	eofnotfound:
	cmp BYTE PTR [eax], 0
	je eoffound
	inc eax
	jmp eofnotfound
	eoffound:
	
	; (✿◠‿◠) my super efficient string comparator
	dec eax
	cmp BYTE PTR [eax], "d"
	jne notenced
	dec eax
	cmp BYTE PTR [eax], "e"
	jne notenced
	dec eax
	cmp BYTE PTR [eax], "t"
	jne notenced
	dec eax
	cmp BYTE PTR [eax], "p"
	jne notenced
	dec eax
	cmp BYTE PTR [eax], "y"
	jne notenced
	dec eax
	cmp BYTE PTR [eax], "r"
	jne notenced
	dec eax
	cmp BYTE PTR [eax], "c"
	jne notenced
	dec eax
	cmp BYTE PTR [eax], "n"
	jne notenced
	dec eax
	cmp BYTE PTR [eax], "e"
	jne notenced
	dec eax
	cmp BYTE PTR [eax], "c"
	jne notenced
	dec eax
	cmp BYTE PTR [eax], "d"
	jne notenced
	dec eax
	cmp BYTE PTR [eax], "_"
	jne notenced
	jmp enced
	
	notenced:
	cmp [action], 0
	je skip
	call get_encryptan
	ret
	
	enced:
	cmp [action], 1
	je skip
	call get_decryptan
	ret
	
	skip:
	ret
dofile endp

get_encryptan proc
	
	;(✿◠‿◠) this applies a simple Electronic Codebook (ECB) to 64 bit chunks of the file with null padding
	; required variables must be set: action, filetowork

	; (✿◠‿◠) Opens file to work on
	push 0
	push FILE_ATTRIBUTE_NORMAL
	push OPEN_EXISTING
	push 0
	push 0
	push GENERIC_ALL
	push offset filetowork
	call CreateFileA
	
	mov [filehandle], eax
	
	keepreading:
	
	; (✿◠‿◠) clears file buffer (in case less than 8 bytes are read)
	mov DWORD PTR [filebuffer], 0
	mov DWORD PTR [filebuffer+4], 0
	
	; (✿◠‿◠) Reads 8 bytes from file (64 bits. the size of a chunk!)
	mov ecx, [filehandle]
	push 0
	push offset readbytes
	push 8
	push offset filebuffer
	push ecx
	call ReadFile
	
	; (✿◠‿◠) escape if end of file
	cmp [readbytes], 0
	je donereading
	
	; (✿◠‿◠) move file pointer back the same number of bytes read
	mov ecx, [filehandle]
	mov edx, [readbytes]
	neg edx
	push FILE_CURRENT
	push 0
	push edx ;negative of read bytes
	push ecx ;file handle
	call SetFilePointer
	
	; (✿◠‿◠) save buffer to eax and ebx registers for left and right side
	mov eax, DWORD PTR [filebuffer]
	mov ebx, DWORD PTR [filebuffer+4]
	
	call blowfish_encrypt ; (✿◠‿◠) Shazam!
	
	; (✿◠‿◠) Save encrypted block back to buffer
	mov DWORD PTR [filebuffer], eax
	mov DWORD PTR [filebuffer+4], ebx
	
	; (✿◠‿◠) Write to file!
	mov ecx, [filehandle]
	push 0
	push offset writtenbytes
	push 8 ; Writes 8 bytes to file. Null padding is left in for simplicity.
	push offset filebuffer
	push ecx
	call WriteFile
	
	cmp [readbytes], 8
	jl donereading

	jmp keepreading
	
	donereading:
	
	; (✿◠‿◠) Close file handle to be able to rename
	mov eax, [filehandle]
	push eax
	call CloseHandle
	
	; (✿◠‿◠) Do the renamey bit
	
	call loadnewfile
	
	lea eax, [newfilename]
	findingend:
	cmp BYTE PTR [eax], 0
	je foundending
	inc eax
	jmp findingend
	foundending:
	
	; (✿◠‿◠) another super efficient string append
	mov BYTE PTR [eax], "_"
	inc eax
	mov BYTE PTR [eax], "d"
	inc eax
	mov BYTE PTR [eax], "c"
	inc eax
	mov BYTE PTR [eax], "e"
	inc eax
	mov BYTE PTR [eax], "n"
	inc eax
	mov BYTE PTR [eax], "c"
	inc eax
	mov BYTE PTR [eax], "r"
	inc eax
	mov BYTE PTR [eax], "y"
	inc eax
	mov BYTE PTR [eax], "p"
	inc eax
	mov BYTE PTR [eax], "t"
	inc eax
	mov BYTE PTR [eax], "e"
	inc eax
	mov BYTE PTR [eax], "d"

	push offset newfilename
	push offset filetowork
	call MoveFile
	
	push offset newfilename
	call StdOut
	push offset crlf
	call StdOut
	
	ret
	
get_encryptan endp

get_decryptan proc
	
	;(✿◠‿◠) This is close to identical as encryptan with the exception of calling blowfish_decrypt

	push 0
	push FILE_ATTRIBUTE_NORMAL
	push OPEN_EXISTING
	push 0
	push 0
	push GENERIC_ALL
	push offset filetowork ;filetowork goes here
	call CreateFileA
	
	mov [filehandle], eax
	
	keepreading:
	
	mov DWORD PTR [filebuffer], 0
	mov DWORD PTR [filebuffer+4], 0
	
	mov ecx, [filehandle]
	push 0
	push offset readbytes
	push 8
	push offset filebuffer
	push ecx
	call ReadFile
	
	cmp [readbytes], 0
	je donereading
	
	mov ecx, [filehandle]
	mov edx, [readbytes]
	neg edx
	push FILE_CURRENT
	push 0
	push edx ;negative of read bytes
	push ecx ;file handle
	call SetFilePointer
			
	mov eax, DWORD PTR [filebuffer]
	mov ebx, DWORD PTR [filebuffer+4]
	
	call blowfish_decrypt ; (✿◠‿◠) Shazaam!
	
	mov DWORD PTR [filebuffer], eax
	mov DWORD PTR [filebuffer+4], ebx

	; (✿◠‿◠) Any code that removes 0 padding can be inserted here

	mov eax, 8
	
	push eax
	mov ecx, [filehandle]
	push 0
	push offset writtenbytes
	push eax ; usually 8 bytes, less for near end of file
	push offset filebuffer
	push ecx
	call WriteFile
	
	pop eax
	cmp eax, 8
	jl donereading
	
	cmp [readbytes], 8
	jl donereading

	jmp keepreading
	
	donereading:
	
	mov eax, [filehandle]
	push eax
	call SetEndOfFile

	mov eax, [filehandle]
	push eax
	call CloseHandle
	
	; (✿◠‿◠) Do the renamey bit
	
	call loadnewfile
	
	lea eax, [newfilename]
	findingend:
	cmp BYTE PTR [eax], 0
	je foundending
	inc eax
	jmp findingend
	foundending:
	
	; (✿◠‿◠) super efficient known-length string truncater
	dec eax
	mov BYTE PTR [eax], 0 ; d
	dec eax
	mov BYTE PTR [eax], 0 ; e
	dec eax
	mov BYTE PTR [eax], 0 ; t
	dec eax
	mov BYTE PTR [eax], 0 ; p
	dec eax
	mov BYTE PTR [eax], 0 ; y
	dec eax
	mov BYTE PTR [eax], 0 ; r
	dec eax
	mov BYTE PTR [eax], 0 ; c
	dec eax
	mov BYTE PTR [eax], 0 ; n
	dec eax
	mov BYTE PTR [eax], 0 ; e
	dec eax
	mov BYTE PTR [eax], 0 ; c
	dec eax
	mov BYTE PTR [eax], 0 ; d
	dec eax
	mov BYTE PTR [eax], 0 ; _
	
	push offset newfilename
	push offset filetowork
	call MoveFile

	push offset newfilename
	call StdOut
	push offset crlf
	call StdOut
	
	ret
	
get_decryptan endp

;----------------------------------->
;    File/Directory traversal!        <
;----------------------------------->

getFiles proc ;(✿◠‿◠) this is where the recursive magic happens

	;required variable: currentdirectory

	lea edx, [currentmaskALL] ; (✿◠‿◠) Blank out and initialize search masks
	blankingALL:
	mov al, [edx]
	cmp al, 0
	je doneblankingALL
	mov BYTE PTR[edx], 0
	inc edx
	jmp blankingALL
	doneblankingALL:
	
	lea edx, [currentmaskTXT]
	blankingTXT:
	mov al, [edx]
	cmp al, 0
	je doneblankingTXT
	mov BYTE PTR[edx], 0
	inc edx
	jmp blankingTXT
	doneblankingTXT:

	lea ecx,[currentdirectory] ; (✿◠‿◠) Make search masks from currentdirectory
	
	lea edx, [currentmaskALL]
	copyingALL:
	mov al, [ecx]
	mov BYTE PTR [edx], al
	cmp al, 0
	je donecopyingALL
	inc ecx
	inc edx
	jmp copyingALL
	donecopyingALL:
	mov BYTE PTR [edx], "\"
	inc edx
	mov BYTE PTR [edx], "*"
	
	lea ecx,[currentdirectory]
	
	lea edx, [currentmaskTXT]
	copyingTXT:
	mov al, [ecx]
	mov BYTE PTR [edx], al
	cmp al, 0
	je donecopyingTXT
	inc ecx
	inc edx
	jmp copyingTXT
	donecopyingTXT:
	
	mov BYTE PTR [edx], "\"
	inc edx
	
	lea ecx, [arg1] ; (✿◠‿◠) Append Mask from Arg2
	appendmask:
	mov al, [ecx]
	cmp al, 0
		je doneappendingmask
	mov BYTE PTR [edx], al
	inc edx
	inc ecx
	jmp appendmask
	doneappendingmask:

	; Text masks ready
	
	push offset foundfile
	push offset currentmaskTXT
	call FindFirstFileA 	
	push eax
	
	cmp eax, INVALID_HANDLE_VALUE
	je nothinghere
	
	mov eax, foundfile.dwFileAttributes ; skip directories
	and eax, FILE_ATTRIBUTE_DIRECTORY
	jnz printerrythang
		
	;  ---------------- FIRST FILE FOUND PUT FILE OPS HERE VVVV

	

	call loadpath ;filetowork is now the filename
	call dofile
	

	
	;  ---------------- End of working with first file  ^^^^^
	
	;find next text files
	printerrythang:
	pop eax
	push eax
	push offset foundfile
	push eax
	call FindNextFileA
	
	cmp eax, 0
	je nothinghere
	
	mov eax, foundfile.dwFileAttributes ; skip directories
	and eax, FILE_ATTRIBUTE_DIRECTORY
	jnz printerrythang
	
	; ----------------- FOUND SECOND FILE PUT FILEOPS HERE VVVVV
	
	
	
	call loadpath
	call dofile
	
	
	;  ---------------- done working on 2nd file ^^^^^^
	
	jmp printerrythang
	
;  ---------------------------------------------- done finding textfiles
	nothinghere:
	call FindClose
; -----------------------------------------------find all directories

	push offset foundfile
	push offset currentmaskALL
	call FindFirstFileA 	
	push eax
	
	cmp eax, INVALID_HANDLE_VALUE ; (✿◠‿◠)  skips search for if there are no directories
	je nothinghere2

	mov eax, foundfile.dwFileAttributes ; (✿◠‿◠)  this is technically uneeded because the first result is usually the directory "." which will be ignored
	and eax, FILE_ATTRIBUTE_DIRECTORY
	jz printerrythang2
	
	; ---------------------------------------- first directory found (ignore it because it's ".")

	
	printerrythang2:
	pop eax
	push eax
	push offset foundfile
	push eax
	call FindNextFileA
	
	cmp eax, 0
	je nothinghere2
	
	mov eax, foundfile.dwFileAttributes
	and eax, FILE_ATTRIBUTE_DIRECTORY
	jz printerrythang2
	
	
	cmp BYTE PTR [foundfile.cFileName], "." ; ignore directories that start with "."
	je printerrythang2
	;  ---------------------------------------- second directory found
	
	; (✿◠‿◠) Preparing to recurse into found directory
	
	lea ecx, [currentdirectory] ; (✿◠‿◠) find null terminator of currentdirectory
	getlastinner:
	mov al, [ecx]
	cmp al, 0
	je gotlastinner
	inc ecx
	jmp getlastinner
	gotlastinner:
	
	mov BYTE PTR [ecx], "\" ; (✿◠‿◠) append a slash
	inc ecx
	
	
	lea edx, foundfile.cFileName ; (✿◠‿◠) append the found directory name
	appendingcurrent:
	mov al, [edx]
	cmp al, 0
	je doneappending
	mov BYTE PTR [ecx], al
	inc ecx
	inc edx
	jmp appendingcurrent
	doneappending:
	
	call getFiles ; (✿◠‿◠) RECURSION!!!

	jmp printerrythang2 ; (✿◠‿◠) Go back and move on to the next directory!
	
	nothinghere2:
	call FindClose; (✿◠‿◠)  gets rid of no longer needed search handle to allow new search handle to be seen
	
	lea ecx, [currentdirectory] ; (✿◠‿◠) now we return currentdirectory to previous state by erasing text up to the last slash (up one level)
	getlast:
	mov al, [ecx]
	cmp al, 0
	je gotlast
	inc ecx
	jmp getlast
	gotlast:
	dec ecx
	mov al, [ecx]
	cmp al, "."
	je doneregressing
	mov BYTE PTR [ecx], 0
	cmp al, "\"
	je doneregressing
	jmp gotlast
	doneregressing:
	
	ret

getFiles endp

;---------------------------
; END OF PROCS
; MAIN CODE BEGINS HERE
;---------------------------

main:

	call initialize_blowfish ; (✿◠‿◠) this program can be modified to have the third argument be a key, for now, the key is hardcoded

	push offset arg1
	push 1
	call GetCL
	
	cmp al, 1
	jne argerror ; (✿◠‿◠) Error because first argument broken
	
	push offset arg3
	push 3
	call GetCL
	
	cmp al, 2
	jne argerror ; (✿◠‿◠) Error because third argument has something going on
	
	push offset arg2
	push 2
	call GetCL
	
	cmp al, 2
	je encryptan ; (✿◠‿◠) blank second argument, will assume encryption
	
	; (✿◠‿◠) a super efficient comparator, the instructions didn't say case sensitive
	cmp BYTE PTR [arg2], "/"
	jne argerror
	cmp BYTE PTR [arg2+1], "r"
	jne argerror
	cmp BYTE PTR [arg2+2], "e"
	jne argerror
	cmp BYTE PTR [arg2+3], "s"
	jne argerror
	cmp BYTE PTR [arg2+4], "t"
	jne argerror
	cmp BYTE PTR [arg2+5], "o"
	jne argerror
	cmp BYTE PTR [arg2+6], "r"
	jne argerror
	cmp BYTE PTR [arg2+7], "e"
	jne argerror
	cmp BYTE PTR [arg2+8], 0
	jne argerror

	jmp decryptan

encryptan:
	mov BYTE PTR [action], 1 ; (✿◠‿◠) Sets encryption flag to 1

decryptan: ; (✿◠‿◠) Encryption flag 0 means decrpy
	
	lea ecx, [currentdirectory]
	mov BYTE PTR [ecx], "." ; (✿◠‿◠) initialize currentdirectory as "."
	call getFiles ; (✿◠‿◠) Here we go! First call into recursive function.
	ret
	
argerror:
	push offset ArgErrorMsg
	call StdOut
	ret

end main