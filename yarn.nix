{fetchurl, linkFarm}: rec {
  offline_cache = linkFarm "offline" packages;
  packages = [

    {
      name = "fontawesome-free-5.12.1.tgz";
      path = fetchurl {
        name = "fontawesome-free-5.12.1.tgz";
        url  = "https://registry.yarnpkg.com/@fortawesome/fontawesome-free/-/fontawesome-free-5.12.1.tgz";
        sha1 = "2a98fea9fbb8a606ddc79a4680034e9d5591c550";
      };
    }

    {
      name = "events-3.0.0.tgz";
      path = fetchurl {
        name = "events-3.0.0.tgz";
        url  = "https://registry.yarnpkg.com/@types/events/-/events-3.0.0.tgz";
        sha1 = "2862f3f58a9a7f7c3e78d79f130dd4d71c25c2a7";
      };
    }

    {
      name = "glob-7.1.1.tgz";
      path = fetchurl {
        name = "glob-7.1.1.tgz";
        url  = "https://registry.yarnpkg.com/@types/glob/-/glob-7.1.1.tgz";
        sha1 = "aa59a1c6e3fbc421e07ccd31a944c30eba521575";
      };
    }

    {
      name = "minimatch-3.0.3.tgz";
      path = fetchurl {
        name = "minimatch-3.0.3.tgz";
        url  = "https://registry.yarnpkg.com/@types/minimatch/-/minimatch-3.0.3.tgz";
        sha1 = "3dca0e3f33b200fc7d1139c0cd96c1268cadfd9d";
      };
    }

    {
      name = "node-13.7.1.tgz";
      path = fetchurl {
        name = "node-13.7.1.tgz";
        url  = "https://registry.yarnpkg.com/@types/node/-/node-13.7.1.tgz";
        sha1 = "238eb34a66431b71d2aaddeaa7db166f25971a0d";
      };
    }

    {
      name = "ast-1.8.5.tgz";
      path = fetchurl {
        name = "ast-1.8.5.tgz";
        url  = "https://registry.yarnpkg.com/@webassemblyjs/ast/-/ast-1.8.5.tgz";
        sha1 = "51b1c5fe6576a34953bf4b253df9f0d490d9e359";
      };
    }

    {
      name = "floating-point-hex-parser-1.8.5.tgz";
      path = fetchurl {
        name = "floating-point-hex-parser-1.8.5.tgz";
        url  = "https://registry.yarnpkg.com/@webassemblyjs/floating-point-hex-parser/-/floating-point-hex-parser-1.8.5.tgz";
        sha1 = "1ba926a2923613edce496fd5b02e8ce8a5f49721";
      };
    }

    {
      name = "helper-api-error-1.8.5.tgz";
      path = fetchurl {
        name = "helper-api-error-1.8.5.tgz";
        url  = "https://registry.yarnpkg.com/@webassemblyjs/helper-api-error/-/helper-api-error-1.8.5.tgz";
        sha1 = "c49dad22f645227c5edb610bdb9697f1aab721f7";
      };
    }

    {
      name = "helper-buffer-1.8.5.tgz";
      path = fetchurl {
        name = "helper-buffer-1.8.5.tgz";
        url  = "https://registry.yarnpkg.com/@webassemblyjs/helper-buffer/-/helper-buffer-1.8.5.tgz";
        sha1 = "fea93e429863dd5e4338555f42292385a653f204";
      };
    }

    {
      name = "helper-code-frame-1.8.5.tgz";
      path = fetchurl {
        name = "helper-code-frame-1.8.5.tgz";
        url  = "https://registry.yarnpkg.com/@webassemblyjs/helper-code-frame/-/helper-code-frame-1.8.5.tgz";
        sha1 = "9a740ff48e3faa3022b1dff54423df9aa293c25e";
      };
    }

    {
      name = "helper-fsm-1.8.5.tgz";
      path = fetchurl {
        name = "helper-fsm-1.8.5.tgz";
        url  = "https://registry.yarnpkg.com/@webassemblyjs/helper-fsm/-/helper-fsm-1.8.5.tgz";
        sha1 = "ba0b7d3b3f7e4733da6059c9332275d860702452";
      };
    }

    {
      name = "helper-module-context-1.8.5.tgz";
      path = fetchurl {
        name = "helper-module-context-1.8.5.tgz";
        url  = "https://registry.yarnpkg.com/@webassemblyjs/helper-module-context/-/helper-module-context-1.8.5.tgz";
        sha1 = "def4b9927b0101dc8cbbd8d1edb5b7b9c82eb245";
      };
    }

    {
      name = "helper-wasm-bytecode-1.8.5.tgz";
      path = fetchurl {
        name = "helper-wasm-bytecode-1.8.5.tgz";
        url  = "https://registry.yarnpkg.com/@webassemblyjs/helper-wasm-bytecode/-/helper-wasm-bytecode-1.8.5.tgz";
        sha1 = "537a750eddf5c1e932f3744206551c91c1b93e61";
      };
    }

    {
      name = "helper-wasm-section-1.8.5.tgz";
      path = fetchurl {
        name = "helper-wasm-section-1.8.5.tgz";
        url  = "https://registry.yarnpkg.com/@webassemblyjs/helper-wasm-section/-/helper-wasm-section-1.8.5.tgz";
        sha1 = "74ca6a6bcbe19e50a3b6b462847e69503e6bfcbf";
      };
    }

    {
      name = "ieee754-1.8.5.tgz";
      path = fetchurl {
        name = "ieee754-1.8.5.tgz";
        url  = "https://registry.yarnpkg.com/@webassemblyjs/ieee754/-/ieee754-1.8.5.tgz";
        sha1 = "712329dbef240f36bf57bd2f7b8fb9bf4154421e";
      };
    }

    {
      name = "leb128-1.8.5.tgz";
      path = fetchurl {
        name = "leb128-1.8.5.tgz";
        url  = "https://registry.yarnpkg.com/@webassemblyjs/leb128/-/leb128-1.8.5.tgz";
        sha1 = "044edeb34ea679f3e04cd4fd9824d5e35767ae10";
      };
    }

    {
      name = "utf8-1.8.5.tgz";
      path = fetchurl {
        name = "utf8-1.8.5.tgz";
        url  = "https://registry.yarnpkg.com/@webassemblyjs/utf8/-/utf8-1.8.5.tgz";
        sha1 = "a8bf3b5d8ffe986c7c1e373ccbdc2a0915f0cedc";
      };
    }

    {
      name = "wasm-edit-1.8.5.tgz";
      path = fetchurl {
        name = "wasm-edit-1.8.5.tgz";
        url  = "https://registry.yarnpkg.com/@webassemblyjs/wasm-edit/-/wasm-edit-1.8.5.tgz";
        sha1 = "962da12aa5acc1c131c81c4232991c82ce56e01a";
      };
    }

    {
      name = "wasm-gen-1.8.5.tgz";
      path = fetchurl {
        name = "wasm-gen-1.8.5.tgz";
        url  = "https://registry.yarnpkg.com/@webassemblyjs/wasm-gen/-/wasm-gen-1.8.5.tgz";
        sha1 = "54840766c2c1002eb64ed1abe720aded714f98bc";
      };
    }

    {
      name = "wasm-opt-1.8.5.tgz";
      path = fetchurl {
        name = "wasm-opt-1.8.5.tgz";
        url  = "https://registry.yarnpkg.com/@webassemblyjs/wasm-opt/-/wasm-opt-1.8.5.tgz";
        sha1 = "b24d9f6ba50394af1349f510afa8ffcb8a63d264";
      };
    }

    {
      name = "wasm-parser-1.8.5.tgz";
      path = fetchurl {
        name = "wasm-parser-1.8.5.tgz";
        url  = "https://registry.yarnpkg.com/@webassemblyjs/wasm-parser/-/wasm-parser-1.8.5.tgz";
        sha1 = "21576f0ec88b91427357b8536383668ef7c66b8d";
      };
    }

    {
      name = "wast-parser-1.8.5.tgz";
      path = fetchurl {
        name = "wast-parser-1.8.5.tgz";
        url  = "https://registry.yarnpkg.com/@webassemblyjs/wast-parser/-/wast-parser-1.8.5.tgz";
        sha1 = "e10eecd542d0e7bd394f6827c49f3df6d4eefb8c";
      };
    }

    {
      name = "wast-printer-1.8.5.tgz";
      path = fetchurl {
        name = "wast-printer-1.8.5.tgz";
        url  = "https://registry.yarnpkg.com/@webassemblyjs/wast-printer/-/wast-printer-1.8.5.tgz";
        sha1 = "114bbc481fd10ca0e23b3560fa812748b0bae5bc";
      };
    }

    {
      name = "ieee754-1.2.0.tgz";
      path = fetchurl {
        name = "ieee754-1.2.0.tgz";
        url  = "https://registry.yarnpkg.com/@xtuc/ieee754/-/ieee754-1.2.0.tgz";
        sha1 = "eef014a3145ae477a1cbc00cd1e552336dceb790";
      };
    }

    {
      name = "long-4.2.2.tgz";
      path = fetchurl {
        name = "long-4.2.2.tgz";
        url  = "https://registry.yarnpkg.com/@xtuc/long/-/long-4.2.2.tgz";
        sha1 = "d291c6a4e97989b5c61d9acf396ae4fe133a718d";
      };
    }

    {
      name = "abbrev-1.1.1.tgz";
      path = fetchurl {
        name = "abbrev-1.1.1.tgz";
        url  = "https://registry.yarnpkg.com/abbrev/-/abbrev-1.1.1.tgz";
        sha1 = "f8f2c887ad10bf67f634f005b6987fed3179aac8";
      };
    }

    {
      name = "accepts-1.3.7.tgz";
      path = fetchurl {
        name = "accepts-1.3.7.tgz";
        url  = "https://registry.yarnpkg.com/accepts/-/accepts-1.3.7.tgz";
        sha1 = "531bc726517a3b2b41f850021c6cc15eaab507cd";
      };
    }

    {
      name = "ace-builds-1.4.8.tgz";
      path = fetchurl {
        name = "ace-builds-1.4.8.tgz";
        url  = "https://registry.yarnpkg.com/ace-builds/-/ace-builds-1.4.8.tgz";
        sha1 = "d14be41d30294a2a12581f0bcfee4b696481ffdd";
      };
    }

    {
      name = "acorn-6.4.0.tgz";
      path = fetchurl {
        name = "acorn-6.4.0.tgz";
        url  = "https://registry.yarnpkg.com/acorn/-/acorn-6.4.0.tgz";
        sha1 = "b659d2ffbafa24baf5db1cdbb2c94a983ecd2784";
      };
    }

    {
      name = "ajv-errors-1.0.1.tgz";
      path = fetchurl {
        name = "ajv-errors-1.0.1.tgz";
        url  = "https://registry.yarnpkg.com/ajv-errors/-/ajv-errors-1.0.1.tgz";
        sha1 = "f35986aceb91afadec4102fbd85014950cefa64d";
      };
    }

    {
      name = "ajv-keywords-3.4.1.tgz";
      path = fetchurl {
        name = "ajv-keywords-3.4.1.tgz";
        url  = "https://registry.yarnpkg.com/ajv-keywords/-/ajv-keywords-3.4.1.tgz";
        sha1 = "ef916e271c64ac12171fd8384eaae6b2345854da";
      };
    }

    {
      name = "ajv-5.5.2.tgz";
      path = fetchurl {
        name = "ajv-5.5.2.tgz";
        url  = "https://registry.yarnpkg.com/ajv/-/ajv-5.5.2.tgz";
        sha1 = "73b5eeca3fab653e3d3f9422b341ad42205dc965";
      };
    }

    {
      name = "ajv-6.11.0.tgz";
      path = fetchurl {
        name = "ajv-6.11.0.tgz";
        url  = "https://registry.yarnpkg.com/ajv/-/ajv-6.11.0.tgz";
        sha1 = "c3607cbc8ae392d8a5a536f25b21f8e5f3f87fe9";
      };
    }

    {
      name = "amdefine-1.0.1.tgz";
      path = fetchurl {
        name = "amdefine-1.0.1.tgz";
        url  = "https://registry.yarnpkg.com/amdefine/-/amdefine-1.0.1.tgz";
        sha1 = "4a5282ac164729e93619bcfd3ad151f817ce91f5";
      };
    }

    {
      name = "ansi-colors-3.2.4.tgz";
      path = fetchurl {
        name = "ansi-colors-3.2.4.tgz";
        url  = "https://registry.yarnpkg.com/ansi-colors/-/ansi-colors-3.2.4.tgz";
        sha1 = "e3a3da4bfbae6c86a9c285625de124a234026fbf";
      };
    }

    {
      name = "ansi-html-0.0.7.tgz";
      path = fetchurl {
        name = "ansi-html-0.0.7.tgz";
        url  = "https://registry.yarnpkg.com/ansi-html/-/ansi-html-0.0.7.tgz";
        sha1 = "813584021962a9e9e6fd039f940d12f56ca7859e";
      };
    }

    {
      name = "ansi-regex-2.1.1.tgz";
      path = fetchurl {
        name = "ansi-regex-2.1.1.tgz";
        url  = "https://registry.yarnpkg.com/ansi-regex/-/ansi-regex-2.1.1.tgz";
        sha1 = "c3b33ab5ee360d86e0e628f0468ae7ef27d654df";
      };
    }

    {
      name = "ansi-regex-3.0.0.tgz";
      path = fetchurl {
        name = "ansi-regex-3.0.0.tgz";
        url  = "https://registry.yarnpkg.com/ansi-regex/-/ansi-regex-3.0.0.tgz";
        sha1 = "ed0317c322064f79466c02966bddb605ab37d998";
      };
    }

    {
      name = "ansi-regex-4.1.0.tgz";
      path = fetchurl {
        name = "ansi-regex-4.1.0.tgz";
        url  = "https://registry.yarnpkg.com/ansi-regex/-/ansi-regex-4.1.0.tgz";
        sha1 = "8b9f8f08cf1acb843756a839ca8c7e3168c51997";
      };
    }

    {
      name = "ansi-styles-2.2.1.tgz";
      path = fetchurl {
        name = "ansi-styles-2.2.1.tgz";
        url  = "https://registry.yarnpkg.com/ansi-styles/-/ansi-styles-2.2.1.tgz";
        sha1 = "b432dd3358b634cf75e1e4664368240533c1ddbe";
      };
    }

    {
      name = "ansi-styles-3.2.1.tgz";
      path = fetchurl {
        name = "ansi-styles-3.2.1.tgz";
        url  = "https://registry.yarnpkg.com/ansi-styles/-/ansi-styles-3.2.1.tgz";
        sha1 = "41fbb20243e50b12be0f04b8dedbf07520ce841d";
      };
    }

    {
      name = "anymatch-2.0.0.tgz";
      path = fetchurl {
        name = "anymatch-2.0.0.tgz";
        url  = "https://registry.yarnpkg.com/anymatch/-/anymatch-2.0.0.tgz";
        sha1 = "bcb24b4f37934d9aa7ac17b4adaf89e7c76ef2eb";
      };
    }

    {
      name = "aproba-1.2.0.tgz";
      path = fetchurl {
        name = "aproba-1.2.0.tgz";
        url  = "https://registry.yarnpkg.com/aproba/-/aproba-1.2.0.tgz";
        sha1 = "6802e6264efd18c790a1b0d517f0f2627bf2c94a";
      };
    }

    {
      name = "are-we-there-yet-1.1.5.tgz";
      path = fetchurl {
        name = "are-we-there-yet-1.1.5.tgz";
        url  = "https://registry.yarnpkg.com/are-we-there-yet/-/are-we-there-yet-1.1.5.tgz";
        sha1 = "4b35c2944f062a8bfcda66410760350fe9ddfc21";
      };
    }

    {
      name = "arr-diff-4.0.0.tgz";
      path = fetchurl {
        name = "arr-diff-4.0.0.tgz";
        url  = "https://registry.yarnpkg.com/arr-diff/-/arr-diff-4.0.0.tgz";
        sha1 = "d6461074febfec71e7e15235761a329a5dc7c520";
      };
    }

    {
      name = "arr-flatten-1.1.0.tgz";
      path = fetchurl {
        name = "arr-flatten-1.1.0.tgz";
        url  = "https://registry.yarnpkg.com/arr-flatten/-/arr-flatten-1.1.0.tgz";
        sha1 = "36048bbff4e7b47e136644316c99669ea5ae91f1";
      };
    }

    {
      name = "arr-union-3.1.0.tgz";
      path = fetchurl {
        name = "arr-union-3.1.0.tgz";
        url  = "https://registry.yarnpkg.com/arr-union/-/arr-union-3.1.0.tgz";
        sha1 = "e39b09aea9def866a8f206e288af63919bae39c4";
      };
    }

    {
      name = "array-find-index-1.0.2.tgz";
      path = fetchurl {
        name = "array-find-index-1.0.2.tgz";
        url  = "https://registry.yarnpkg.com/array-find-index/-/array-find-index-1.0.2.tgz";
        sha1 = "df010aa1287e164bbda6f9723b0a96a1ec4187a1";
      };
    }

    {
      name = "array-flatten-1.1.1.tgz";
      path = fetchurl {
        name = "array-flatten-1.1.1.tgz";
        url  = "https://registry.yarnpkg.com/array-flatten/-/array-flatten-1.1.1.tgz";
        sha1 = "9a5f699051b1e7073328f2a008968b64ea2955d2";
      };
    }

    {
      name = "array-flatten-2.1.2.tgz";
      path = fetchurl {
        name = "array-flatten-2.1.2.tgz";
        url  = "https://registry.yarnpkg.com/array-flatten/-/array-flatten-2.1.2.tgz";
        sha1 = "24ef80a28c1a893617e2149b0c6d0d788293b099";
      };
    }

    {
      name = "array-union-1.0.2.tgz";
      path = fetchurl {
        name = "array-union-1.0.2.tgz";
        url  = "https://registry.yarnpkg.com/array-union/-/array-union-1.0.2.tgz";
        sha1 = "9a34410e4f4e3da23dea375be5be70f24778ec39";
      };
    }

    {
      name = "array-uniq-1.0.3.tgz";
      path = fetchurl {
        name = "array-uniq-1.0.3.tgz";
        url  = "https://registry.yarnpkg.com/array-uniq/-/array-uniq-1.0.3.tgz";
        sha1 = "af6ac877a25cc7f74e058894753858dfdb24fdb6";
      };
    }

    {
      name = "array-unique-0.3.2.tgz";
      path = fetchurl {
        name = "array-unique-0.3.2.tgz";
        url  = "https://registry.yarnpkg.com/array-unique/-/array-unique-0.3.2.tgz";
        sha1 = "a894b75d4bc4f6cd679ef3244a9fd8f46ae2d428";
      };
    }

    {
      name = "arrify-1.0.1.tgz";
      path = fetchurl {
        name = "arrify-1.0.1.tgz";
        url  = "https://registry.yarnpkg.com/arrify/-/arrify-1.0.1.tgz";
        sha1 = "898508da2226f380df904728456849c1501a4b0d";
      };
    }

    {
      name = "asn1.js-4.10.1.tgz";
      path = fetchurl {
        name = "asn1.js-4.10.1.tgz";
        url  = "https://registry.yarnpkg.com/asn1.js/-/asn1.js-4.10.1.tgz";
        sha1 = "b9c2bf5805f1e64aadeed6df3a2bfafb5a73f5a0";
      };
    }

    {
      name = "asn1-0.2.4.tgz";
      path = fetchurl {
        name = "asn1-0.2.4.tgz";
        url  = "https://registry.yarnpkg.com/asn1/-/asn1-0.2.4.tgz";
        sha1 = "8d2475dfab553bb33e77b54e59e880bb8ce23136";
      };
    }

    {
      name = "assert-plus-1.0.0.tgz";
      path = fetchurl {
        name = "assert-plus-1.0.0.tgz";
        url  = "https://registry.yarnpkg.com/assert-plus/-/assert-plus-1.0.0.tgz";
        sha1 = "f12e0f3c5d77b0b1cdd9146942e4e96c1e4dd525";
      };
    }

    {
      name = "assert-1.5.0.tgz";
      path = fetchurl {
        name = "assert-1.5.0.tgz";
        url  = "https://registry.yarnpkg.com/assert/-/assert-1.5.0.tgz";
        sha1 = "55c109aaf6e0aefdb3dc4b71240c70bf574b18eb";
      };
    }

    {
      name = "assign-symbols-1.0.0.tgz";
      path = fetchurl {
        name = "assign-symbols-1.0.0.tgz";
        url  = "https://registry.yarnpkg.com/assign-symbols/-/assign-symbols-1.0.0.tgz";
        sha1 = "59667f41fadd4f20ccbc2bb96b8d4f7f78ec0367";
      };
    }

    {
      name = "async-each-1.0.3.tgz";
      path = fetchurl {
        name = "async-each-1.0.3.tgz";
        url  = "https://registry.yarnpkg.com/async-each/-/async-each-1.0.3.tgz";
        sha1 = "b727dbf87d7651602f06f4d4ac387f47d91b0cbf";
      };
    }

    {
      name = "async-foreach-0.1.3.tgz";
      path = fetchurl {
        name = "async-foreach-0.1.3.tgz";
        url  = "https://registry.yarnpkg.com/async-foreach/-/async-foreach-0.1.3.tgz";
        sha1 = "36121f845c0578172de419a97dbeb1d16ec34542";
      };
    }

    {
      name = "async-limiter-1.0.1.tgz";
      path = fetchurl {
        name = "async-limiter-1.0.1.tgz";
        url  = "https://registry.yarnpkg.com/async-limiter/-/async-limiter-1.0.1.tgz";
        sha1 = "dd379e94f0db8310b08291f9d64c3209766617fd";
      };
    }

    {
      name = "async-2.6.3.tgz";
      path = fetchurl {
        name = "async-2.6.3.tgz";
        url  = "https://registry.yarnpkg.com/async/-/async-2.6.3.tgz";
        sha1 = "d72625e2344a3656e3a3ad4fa749fa83299d82ff";
      };
    }

    {
      name = "asynckit-0.4.0.tgz";
      path = fetchurl {
        name = "asynckit-0.4.0.tgz";
        url  = "https://registry.yarnpkg.com/asynckit/-/asynckit-0.4.0.tgz";
        sha1 = "c79ed97f7f34cb8f2ba1bc9790bcc366474b4b79";
      };
    }

    {
      name = "atob-2.1.2.tgz";
      path = fetchurl {
        name = "atob-2.1.2.tgz";
        url  = "https://registry.yarnpkg.com/atob/-/atob-2.1.2.tgz";
        sha1 = "6d9517eb9e030d2436666651e86bd9f6f13533c9";
      };
    }

    {
      name = "aws-sign2-0.7.0.tgz";
      path = fetchurl {
        name = "aws-sign2-0.7.0.tgz";
        url  = "https://registry.yarnpkg.com/aws-sign2/-/aws-sign2-0.7.0.tgz";
        sha1 = "b46e890934a9591f2d2f6f86d7e6a9f1b3fe76a8";
      };
    }

    {
      name = "aws4-1.9.1.tgz";
      path = fetchurl {
        name = "aws4-1.9.1.tgz";
        url  = "https://registry.yarnpkg.com/aws4/-/aws4-1.9.1.tgz";
        sha1 = "7e33d8f7d449b3f673cd72deb9abdc552dbe528e";
      };
    }

    {
      name = "babel-code-frame-6.26.0.tgz";
      path = fetchurl {
        name = "babel-code-frame-6.26.0.tgz";
        url  = "https://registry.yarnpkg.com/babel-code-frame/-/babel-code-frame-6.26.0.tgz";
        sha1 = "63fd43f7dc1e3bb7ce35947db8fe369a3f58c74b";
      };
    }

    {
      name = "balanced-match-1.0.0.tgz";
      path = fetchurl {
        name = "balanced-match-1.0.0.tgz";
        url  = "https://registry.yarnpkg.com/balanced-match/-/balanced-match-1.0.0.tgz";
        sha1 = "89b4d199ab2bee49de164ea02b89ce462d71b767";
      };
    }

    {
      name = "base64-js-1.3.1.tgz";
      path = fetchurl {
        name = "base64-js-1.3.1.tgz";
        url  = "https://registry.yarnpkg.com/base64-js/-/base64-js-1.3.1.tgz";
        sha1 = "58ece8cb75dd07e71ed08c736abc5fac4dbf8df1";
      };
    }

    {
      name = "base-0.11.2.tgz";
      path = fetchurl {
        name = "base-0.11.2.tgz";
        url  = "https://registry.yarnpkg.com/base/-/base-0.11.2.tgz";
        sha1 = "7bde5ced145b6d551a90db87f83c558b4eb48a8f";
      };
    }

    {
      name = "batch-0.6.1.tgz";
      path = fetchurl {
        name = "batch-0.6.1.tgz";
        url  = "https://registry.yarnpkg.com/batch/-/batch-0.6.1.tgz";
        sha1 = "dc34314f4e679318093fc760272525f94bf25c16";
      };
    }

    {
      name = "bcrypt-pbkdf-1.0.2.tgz";
      path = fetchurl {
        name = "bcrypt-pbkdf-1.0.2.tgz";
        url  = "https://registry.yarnpkg.com/bcrypt-pbkdf/-/bcrypt-pbkdf-1.0.2.tgz";
        sha1 = "a4301d389b6a43f9b67ff3ca11a3f6637e360e9e";
      };
    }

    {
      name = "big.js-3.2.0.tgz";
      path = fetchurl {
        name = "big.js-3.2.0.tgz";
        url  = "https://registry.yarnpkg.com/big.js/-/big.js-3.2.0.tgz";
        sha1 = "a5fc298b81b9e0dca2e458824784b65c52ba588e";
      };
    }

    {
      name = "big.js-5.2.2.tgz";
      path = fetchurl {
        name = "big.js-5.2.2.tgz";
        url  = "https://registry.yarnpkg.com/big.js/-/big.js-5.2.2.tgz";
        sha1 = "65f0af382f578bcdc742bd9c281e9cb2d7768328";
      };
    }

    {
      name = "binary-extensions-1.13.1.tgz";
      path = fetchurl {
        name = "binary-extensions-1.13.1.tgz";
        url  = "https://registry.yarnpkg.com/binary-extensions/-/binary-extensions-1.13.1.tgz";
        sha1 = "598afe54755b2868a5330d2aff9d4ebb53209b65";
      };
    }

    {
      name = "bindings-1.5.0.tgz";
      path = fetchurl {
        name = "bindings-1.5.0.tgz";
        url  = "https://registry.yarnpkg.com/bindings/-/bindings-1.5.0.tgz";
        sha1 = "10353c9e945334bc0511a6d90b38fbc7c9c504df";
      };
    }

    {
      name = "block-stream-0.0.9.tgz";
      path = fetchurl {
        name = "block-stream-0.0.9.tgz";
        url  = "https://registry.yarnpkg.com/block-stream/-/block-stream-0.0.9.tgz";
        sha1 = "13ebfe778a03205cfe03751481ebb4b3300c126a";
      };
    }

    {
      name = "bluebird-3.7.2.tgz";
      path = fetchurl {
        name = "bluebird-3.7.2.tgz";
        url  = "https://registry.yarnpkg.com/bluebird/-/bluebird-3.7.2.tgz";
        sha1 = "9f229c15be272454ffa973ace0dbee79a1b0c36f";
      };
    }

    {
      name = "bn.js-4.11.8.tgz";
      path = fetchurl {
        name = "bn.js-4.11.8.tgz";
        url  = "https://registry.yarnpkg.com/bn.js/-/bn.js-4.11.8.tgz";
        sha1 = "2cde09eb5ee341f484746bb0309b3253b1b1442f";
      };
    }

    {
      name = "body-parser-1.19.0.tgz";
      path = fetchurl {
        name = "body-parser-1.19.0.tgz";
        url  = "https://registry.yarnpkg.com/body-parser/-/body-parser-1.19.0.tgz";
        sha1 = "96b2709e57c9c4e09a6fd66a8fd979844f69f08a";
      };
    }

    {
      name = "bonjour-3.5.0.tgz";
      path = fetchurl {
        name = "bonjour-3.5.0.tgz";
        url  = "https://registry.yarnpkg.com/bonjour/-/bonjour-3.5.0.tgz";
        sha1 = "8e890a183d8ee9a2393b3844c691a42bcf7bc9f5";
      };
    }

    {
      name = "boolbase-1.0.0.tgz";
      path = fetchurl {
        name = "boolbase-1.0.0.tgz";
        url  = "https://registry.yarnpkg.com/boolbase/-/boolbase-1.0.0.tgz";
        sha1 = "68dff5fbe60c51eb37725ea9e3ed310dcc1e776e";
      };
    }

    {
      name = "bootstrap-4.4.1.tgz";
      path = fetchurl {
        name = "bootstrap-4.4.1.tgz";
        url  = "https://registry.yarnpkg.com/bootstrap/-/bootstrap-4.4.1.tgz";
        sha1 = "8582960eea0c5cd2bede84d8b0baf3789c3e8b01";
      };
    }

    {
      name = "brace-expansion-1.1.11.tgz";
      path = fetchurl {
        name = "brace-expansion-1.1.11.tgz";
        url  = "https://registry.yarnpkg.com/brace-expansion/-/brace-expansion-1.1.11.tgz";
        sha1 = "3c7fcbf529d87226f3d2f52b966ff5271eb441dd";
      };
    }

    {
      name = "braces-2.3.2.tgz";
      path = fetchurl {
        name = "braces-2.3.2.tgz";
        url  = "https://registry.yarnpkg.com/braces/-/braces-2.3.2.tgz";
        sha1 = "5979fd3f14cd531565e5fa2df1abfff1dfaee729";
      };
    }

    {
      name = "brorand-1.1.0.tgz";
      path = fetchurl {
        name = "brorand-1.1.0.tgz";
        url  = "https://registry.yarnpkg.com/brorand/-/brorand-1.1.0.tgz";
        sha1 = "12c25efe40a45e3c323eb8675a0a0ce57b22371f";
      };
    }

    {
      name = "browserify-aes-1.2.0.tgz";
      path = fetchurl {
        name = "browserify-aes-1.2.0.tgz";
        url  = "https://registry.yarnpkg.com/browserify-aes/-/browserify-aes-1.2.0.tgz";
        sha1 = "326734642f403dabc3003209853bb70ad428ef48";
      };
    }

    {
      name = "browserify-cipher-1.0.1.tgz";
      path = fetchurl {
        name = "browserify-cipher-1.0.1.tgz";
        url  = "https://registry.yarnpkg.com/browserify-cipher/-/browserify-cipher-1.0.1.tgz";
        sha1 = "8d6474c1b870bfdabcd3bcfcc1934a10e94f15f0";
      };
    }

    {
      name = "browserify-des-1.0.2.tgz";
      path = fetchurl {
        name = "browserify-des-1.0.2.tgz";
        url  = "https://registry.yarnpkg.com/browserify-des/-/browserify-des-1.0.2.tgz";
        sha1 = "3af4f1f59839403572f1c66204375f7a7f703e9c";
      };
    }

    {
      name = "browserify-rsa-4.0.1.tgz";
      path = fetchurl {
        name = "browserify-rsa-4.0.1.tgz";
        url  = "https://registry.yarnpkg.com/browserify-rsa/-/browserify-rsa-4.0.1.tgz";
        sha1 = "21e0abfaf6f2029cf2fafb133567a701d4135524";
      };
    }

    {
      name = "browserify-sign-4.0.4.tgz";
      path = fetchurl {
        name = "browserify-sign-4.0.4.tgz";
        url  = "https://registry.yarnpkg.com/browserify-sign/-/browserify-sign-4.0.4.tgz";
        sha1 = "aa4eb68e5d7b658baa6bf6a57e630cbd7a93d298";
      };
    }

    {
      name = "browserify-zlib-0.2.0.tgz";
      path = fetchurl {
        name = "browserify-zlib-0.2.0.tgz";
        url  = "https://registry.yarnpkg.com/browserify-zlib/-/browserify-zlib-0.2.0.tgz";
        sha1 = "2869459d9aa3be245fe8fe2ca1f46e2e7f54d73f";
      };
    }

    {
      name = "buffer-from-1.1.1.tgz";
      path = fetchurl {
        name = "buffer-from-1.1.1.tgz";
        url  = "https://registry.yarnpkg.com/buffer-from/-/buffer-from-1.1.1.tgz";
        sha1 = "32713bc028f75c02fdb710d7c7bcec1f2c6070ef";
      };
    }

    {
      name = "buffer-indexof-1.1.1.tgz";
      path = fetchurl {
        name = "buffer-indexof-1.1.1.tgz";
        url  = "https://registry.yarnpkg.com/buffer-indexof/-/buffer-indexof-1.1.1.tgz";
        sha1 = "52fabcc6a606d1a00302802648ef68f639da268c";
      };
    }

    {
      name = "buffer-xor-1.0.3.tgz";
      path = fetchurl {
        name = "buffer-xor-1.0.3.tgz";
        url  = "https://registry.yarnpkg.com/buffer-xor/-/buffer-xor-1.0.3.tgz";
        sha1 = "26e61ed1422fb70dd42e6e36729ed51d855fe8d9";
      };
    }

    {
      name = "buffer-4.9.2.tgz";
      path = fetchurl {
        name = "buffer-4.9.2.tgz";
        url  = "https://registry.yarnpkg.com/buffer/-/buffer-4.9.2.tgz";
        sha1 = "230ead344002988644841ab0244af8c44bbe3ef8";
      };
    }

    {
      name = "builtin-status-codes-3.0.0.tgz";
      path = fetchurl {
        name = "builtin-status-codes-3.0.0.tgz";
        url  = "https://registry.yarnpkg.com/builtin-status-codes/-/builtin-status-codes-3.0.0.tgz";
        sha1 = "85982878e21b98e1c66425e03d0174788f569ee8";
      };
    }

    {
      name = "bytes-3.0.0.tgz";
      path = fetchurl {
        name = "bytes-3.0.0.tgz";
        url  = "https://registry.yarnpkg.com/bytes/-/bytes-3.0.0.tgz";
        sha1 = "d32815404d689699f85a4ea4fa8755dd13a96048";
      };
    }

    {
      name = "bytes-3.1.0.tgz";
      path = fetchurl {
        name = "bytes-3.1.0.tgz";
        url  = "https://registry.yarnpkg.com/bytes/-/bytes-3.1.0.tgz";
        sha1 = "f6cf7933a360e0588fa9fde85651cdc7f805d1f6";
      };
    }

    {
      name = "cacache-12.0.3.tgz";
      path = fetchurl {
        name = "cacache-12.0.3.tgz";
        url  = "https://registry.yarnpkg.com/cacache/-/cacache-12.0.3.tgz";
        sha1 = "be99abba4e1bf5df461cd5a2c1071fc432573390";
      };
    }

    {
      name = "cache-base-1.0.1.tgz";
      path = fetchurl {
        name = "cache-base-1.0.1.tgz";
        url  = "https://registry.yarnpkg.com/cache-base/-/cache-base-1.0.1.tgz";
        sha1 = "0a7f46416831c8b662ee36fe4e7c59d76f666ab2";
      };
    }

    {
      name = "camel-case-3.0.0.tgz";
      path = fetchurl {
        name = "camel-case-3.0.0.tgz";
        url  = "https://registry.yarnpkg.com/camel-case/-/camel-case-3.0.0.tgz";
        sha1 = "ca3c3688a4e9cf3a4cda777dc4dcbc713249cf73";
      };
    }

    {
      name = "camelcase-keys-2.1.0.tgz";
      path = fetchurl {
        name = "camelcase-keys-2.1.0.tgz";
        url  = "https://registry.yarnpkg.com/camelcase-keys/-/camelcase-keys-2.1.0.tgz";
        sha1 = "308beeaffdf28119051efa1d932213c91b8f92e7";
      };
    }

    {
      name = "camelcase-2.1.1.tgz";
      path = fetchurl {
        name = "camelcase-2.1.1.tgz";
        url  = "https://registry.yarnpkg.com/camelcase/-/camelcase-2.1.1.tgz";
        sha1 = "7c1d16d679a1bbe59ca02cacecfb011e201f5a1f";
      };
    }

    {
      name = "camelcase-3.0.0.tgz";
      path = fetchurl {
        name = "camelcase-3.0.0.tgz";
        url  = "https://registry.yarnpkg.com/camelcase/-/camelcase-3.0.0.tgz";
        sha1 = "32fc4b9fcdaf845fcdf7e73bb97cac2261f0ab0a";
      };
    }

    {
      name = "camelcase-5.3.1.tgz";
      path = fetchurl {
        name = "camelcase-5.3.1.tgz";
        url  = "https://registry.yarnpkg.com/camelcase/-/camelcase-5.3.1.tgz";
        sha1 = "e3c9b31569e106811df242f715725a1f4c494320";
      };
    }

    {
      name = "caseless-0.12.0.tgz";
      path = fetchurl {
        name = "caseless-0.12.0.tgz";
        url  = "https://registry.yarnpkg.com/caseless/-/caseless-0.12.0.tgz";
        sha1 = "1b681c21ff84033c826543090689420d187151dc";
      };
    }

    {
      name = "chalk-2.4.2.tgz";
      path = fetchurl {
        name = "chalk-2.4.2.tgz";
        url  = "https://registry.yarnpkg.com/chalk/-/chalk-2.4.2.tgz";
        sha1 = "cd42541677a54333cf541a49108c1432b44c9424";
      };
    }

    {
      name = "chalk-1.1.3.tgz";
      path = fetchurl {
        name = "chalk-1.1.3.tgz";
        url  = "https://registry.yarnpkg.com/chalk/-/chalk-1.1.3.tgz";
        sha1 = "a8115c55e4a702fe4d150abd3872822a7e09fc98";
      };
    }

    {
      name = "chokidar-2.1.8.tgz";
      path = fetchurl {
        name = "chokidar-2.1.8.tgz";
        url  = "https://registry.yarnpkg.com/chokidar/-/chokidar-2.1.8.tgz";
        sha1 = "804b3a7b6a99358c3c5c61e71d8728f041cff917";
      };
    }

    {
      name = "chownr-1.1.4.tgz";
      path = fetchurl {
        name = "chownr-1.1.4.tgz";
        url  = "https://registry.yarnpkg.com/chownr/-/chownr-1.1.4.tgz";
        sha1 = "6fc9d7b42d32a583596337666e7d08084da2cc6b";
      };
    }

    {
      name = "chrome-trace-event-1.0.2.tgz";
      path = fetchurl {
        name = "chrome-trace-event-1.0.2.tgz";
        url  = "https://registry.yarnpkg.com/chrome-trace-event/-/chrome-trace-event-1.0.2.tgz";
        sha1 = "234090ee97c7d4ad1a2c4beae27505deffc608a4";
      };
    }

    {
      name = "cipher-base-1.0.4.tgz";
      path = fetchurl {
        name = "cipher-base-1.0.4.tgz";
        url  = "https://registry.yarnpkg.com/cipher-base/-/cipher-base-1.0.4.tgz";
        sha1 = "8760e4ecc272f4c363532f926d874aae2c1397de";
      };
    }

    {
      name = "class-utils-0.3.6.tgz";
      path = fetchurl {
        name = "class-utils-0.3.6.tgz";
        url  = "https://registry.yarnpkg.com/class-utils/-/class-utils-0.3.6.tgz";
        sha1 = "f93369ae8b9a7ce02fd41faad0ca83033190c463";
      };
    }

    {
      name = "clean-css-4.2.3.tgz";
      path = fetchurl {
        name = "clean-css-4.2.3.tgz";
        url  = "https://registry.yarnpkg.com/clean-css/-/clean-css-4.2.3.tgz";
        sha1 = "507b5de7d97b48ee53d84adb0160ff6216380f78";
      };
    }

    {
      name = "cliui-3.2.0.tgz";
      path = fetchurl {
        name = "cliui-3.2.0.tgz";
        url  = "https://registry.yarnpkg.com/cliui/-/cliui-3.2.0.tgz";
        sha1 = "120601537a916d29940f934da3b48d585a39213d";
      };
    }

    {
      name = "cliui-4.1.0.tgz";
      path = fetchurl {
        name = "cliui-4.1.0.tgz";
        url  = "https://registry.yarnpkg.com/cliui/-/cliui-4.1.0.tgz";
        sha1 = "348422dbe82d800b3022eef4f6ac10bf2e4d1b49";
      };
    }

    {
      name = "cliui-5.0.0.tgz";
      path = fetchurl {
        name = "cliui-5.0.0.tgz";
        url  = "https://registry.yarnpkg.com/cliui/-/cliui-5.0.0.tgz";
        sha1 = "deefcfdb2e800784aa34f46fa08e06851c7bbbc5";
      };
    }

    {
      name = "clone-deep-4.0.1.tgz";
      path = fetchurl {
        name = "clone-deep-4.0.1.tgz";
        url  = "https://registry.yarnpkg.com/clone-deep/-/clone-deep-4.0.1.tgz";
        sha1 = "c19fd9bdbbf85942b4fd979c84dcf7d5f07c2387";
      };
    }

    {
      name = "co-4.6.0.tgz";
      path = fetchurl {
        name = "co-4.6.0.tgz";
        url  = "https://registry.yarnpkg.com/co/-/co-4.6.0.tgz";
        sha1 = "6ea6bdf3d853ae54ccb8e47bfa0bf3f9031fb184";
      };
    }

    {
      name = "code-point-at-1.1.0.tgz";
      path = fetchurl {
        name = "code-point-at-1.1.0.tgz";
        url  = "https://registry.yarnpkg.com/code-point-at/-/code-point-at-1.1.0.tgz";
        sha1 = "0d070b4d043a5bea33a2f1a40e2edb3d9a4ccf77";
      };
    }

    {
      name = "collection-visit-1.0.0.tgz";
      path = fetchurl {
        name = "collection-visit-1.0.0.tgz";
        url  = "https://registry.yarnpkg.com/collection-visit/-/collection-visit-1.0.0.tgz";
        sha1 = "4bc0373c164bc3291b4d368c829cf1a80a59dca0";
      };
    }

    {
      name = "color-convert-1.9.3.tgz";
      path = fetchurl {
        name = "color-convert-1.9.3.tgz";
        url  = "https://registry.yarnpkg.com/color-convert/-/color-convert-1.9.3.tgz";
        sha1 = "bb71850690e1f136567de629d2d5471deda4c1e8";
      };
    }

    {
      name = "color-name-1.1.3.tgz";
      path = fetchurl {
        name = "color-name-1.1.3.tgz";
        url  = "https://registry.yarnpkg.com/color-name/-/color-name-1.1.3.tgz";
        sha1 = "a7d0558bd89c42f795dd42328f740831ca53bc25";
      };
    }

    {
      name = "combined-stream-1.0.8.tgz";
      path = fetchurl {
        name = "combined-stream-1.0.8.tgz";
        url  = "https://registry.yarnpkg.com/combined-stream/-/combined-stream-1.0.8.tgz";
        sha1 = "c3d45a8b34fd730631a110a8a2520682b31d5a7f";
      };
    }

    {
      name = "commander-2.17.1.tgz";
      path = fetchurl {
        name = "commander-2.17.1.tgz";
        url  = "https://registry.yarnpkg.com/commander/-/commander-2.17.1.tgz";
        sha1 = "bd77ab7de6de94205ceacc72f1716d29f20a77bf";
      };
    }

    {
      name = "commander-2.20.3.tgz";
      path = fetchurl {
        name = "commander-2.20.3.tgz";
        url  = "https://registry.yarnpkg.com/commander/-/commander-2.20.3.tgz";
        sha1 = "fd485e84c03eb4881c20722ba48035e8531aeb33";
      };
    }

    {
      name = "commander-2.19.0.tgz";
      path = fetchurl {
        name = "commander-2.19.0.tgz";
        url  = "https://registry.yarnpkg.com/commander/-/commander-2.19.0.tgz";
        sha1 = "f6198aa84e5b83c46054b94ddedbfed5ee9ff12a";
      };
    }

    {
      name = "commondir-1.0.1.tgz";
      path = fetchurl {
        name = "commondir-1.0.1.tgz";
        url  = "https://registry.yarnpkg.com/commondir/-/commondir-1.0.1.tgz";
        sha1 = "ddd800da0c66127393cca5950ea968a3aaf1253b";
      };
    }

    {
      name = "component-emitter-1.3.0.tgz";
      path = fetchurl {
        name = "component-emitter-1.3.0.tgz";
        url  = "https://registry.yarnpkg.com/component-emitter/-/component-emitter-1.3.0.tgz";
        sha1 = "16e4070fba8ae29b679f2215853ee181ab2eabc0";
      };
    }

    {
      name = "compressible-2.0.18.tgz";
      path = fetchurl {
        name = "compressible-2.0.18.tgz";
        url  = "https://registry.yarnpkg.com/compressible/-/compressible-2.0.18.tgz";
        sha1 = "af53cca6b070d4c3c0750fbd77286a6d7cc46fba";
      };
    }

    {
      name = "compression-1.7.4.tgz";
      path = fetchurl {
        name = "compression-1.7.4.tgz";
        url  = "https://registry.yarnpkg.com/compression/-/compression-1.7.4.tgz";
        sha1 = "95523eff170ca57c29a0ca41e6fe131f41e5bb8f";
      };
    }

    {
      name = "concat-map-0.0.1.tgz";
      path = fetchurl {
        name = "concat-map-0.0.1.tgz";
        url  = "https://registry.yarnpkg.com/concat-map/-/concat-map-0.0.1.tgz";
        sha1 = "d8a96bd77fd68df7793a73036a3ba0d5405d477b";
      };
    }

    {
      name = "concat-stream-1.6.2.tgz";
      path = fetchurl {
        name = "concat-stream-1.6.2.tgz";
        url  = "https://registry.yarnpkg.com/concat-stream/-/concat-stream-1.6.2.tgz";
        sha1 = "904bdf194cd3122fc675c77fc4ac3d4ff0fd1a34";
      };
    }

    {
      name = "connect-history-api-fallback-1.6.0.tgz";
      path = fetchurl {
        name = "connect-history-api-fallback-1.6.0.tgz";
        url  = "https://registry.yarnpkg.com/connect-history-api-fallback/-/connect-history-api-fallback-1.6.0.tgz";
        sha1 = "8b32089359308d111115d81cad3fceab888f97bc";
      };
    }

    {
      name = "console-browserify-1.2.0.tgz";
      path = fetchurl {
        name = "console-browserify-1.2.0.tgz";
        url  = "https://registry.yarnpkg.com/console-browserify/-/console-browserify-1.2.0.tgz";
        sha1 = "67063cef57ceb6cf4993a2ab3a55840ae8c49336";
      };
    }

    {
      name = "console-control-strings-1.1.0.tgz";
      path = fetchurl {
        name = "console-control-strings-1.1.0.tgz";
        url  = "https://registry.yarnpkg.com/console-control-strings/-/console-control-strings-1.1.0.tgz";
        sha1 = "3d7cf4464db6446ea644bf4b39507f9851008e8e";
      };
    }

    {
      name = "constants-browserify-1.0.0.tgz";
      path = fetchurl {
        name = "constants-browserify-1.0.0.tgz";
        url  = "https://registry.yarnpkg.com/constants-browserify/-/constants-browserify-1.0.0.tgz";
        sha1 = "c20b96d8c617748aaf1c16021760cd27fcb8cb75";
      };
    }

    {
      name = "content-disposition-0.5.3.tgz";
      path = fetchurl {
        name = "content-disposition-0.5.3.tgz";
        url  = "https://registry.yarnpkg.com/content-disposition/-/content-disposition-0.5.3.tgz";
        sha1 = "e130caf7e7279087c5616c2007d0485698984fbd";
      };
    }

    {
      name = "content-type-1.0.4.tgz";
      path = fetchurl {
        name = "content-type-1.0.4.tgz";
        url  = "https://registry.yarnpkg.com/content-type/-/content-type-1.0.4.tgz";
        sha1 = "e138cc75e040c727b1966fe5e5f8c9aee256fe3b";
      };
    }

    {
      name = "cookie-signature-1.0.6.tgz";
      path = fetchurl {
        name = "cookie-signature-1.0.6.tgz";
        url  = "https://registry.yarnpkg.com/cookie-signature/-/cookie-signature-1.0.6.tgz";
        sha1 = "e303a882b342cc3ee8ca513a79999734dab3ae2c";
      };
    }

    {
      name = "cookie-0.4.0.tgz";
      path = fetchurl {
        name = "cookie-0.4.0.tgz";
        url  = "https://registry.yarnpkg.com/cookie/-/cookie-0.4.0.tgz";
        sha1 = "beb437e7022b3b6d49019d088665303ebe9c14ba";
      };
    }

    {
      name = "copy-concurrently-1.0.5.tgz";
      path = fetchurl {
        name = "copy-concurrently-1.0.5.tgz";
        url  = "https://registry.yarnpkg.com/copy-concurrently/-/copy-concurrently-1.0.5.tgz";
        sha1 = "92297398cae34937fcafd6ec8139c18051f0b5e0";
      };
    }

    {
      name = "copy-descriptor-0.1.1.tgz";
      path = fetchurl {
        name = "copy-descriptor-0.1.1.tgz";
        url  = "https://registry.yarnpkg.com/copy-descriptor/-/copy-descriptor-0.1.1.tgz";
        sha1 = "676f6eb3c39997c2ee1ac3a924fd6124748f578d";
      };
    }

    {
      name = "core-util-is-1.0.2.tgz";
      path = fetchurl {
        name = "core-util-is-1.0.2.tgz";
        url  = "https://registry.yarnpkg.com/core-util-is/-/core-util-is-1.0.2.tgz";
        sha1 = "b5fd54220aa2bc5ab57aab7140c940754503c1a7";
      };
    }

    {
      name = "create-ecdh-4.0.3.tgz";
      path = fetchurl {
        name = "create-ecdh-4.0.3.tgz";
        url  = "https://registry.yarnpkg.com/create-ecdh/-/create-ecdh-4.0.3.tgz";
        sha1 = "c9111b6f33045c4697f144787f9254cdc77c45ff";
      };
    }

    {
      name = "create-hash-1.2.0.tgz";
      path = fetchurl {
        name = "create-hash-1.2.0.tgz";
        url  = "https://registry.yarnpkg.com/create-hash/-/create-hash-1.2.0.tgz";
        sha1 = "889078af11a63756bcfb59bd221996be3a9ef196";
      };
    }

    {
      name = "create-hmac-1.1.7.tgz";
      path = fetchurl {
        name = "create-hmac-1.1.7.tgz";
        url  = "https://registry.yarnpkg.com/create-hmac/-/create-hmac-1.1.7.tgz";
        sha1 = "69170c78b3ab957147b2b8b04572e47ead2243ff";
      };
    }

    {
      name = "cross-spawn-6.0.5.tgz";
      path = fetchurl {
        name = "cross-spawn-6.0.5.tgz";
        url  = "https://registry.yarnpkg.com/cross-spawn/-/cross-spawn-6.0.5.tgz";
        sha1 = "4a5ec7c64dfae22c3a14124dbacdee846d80cbc4";
      };
    }

    {
      name = "cross-spawn-3.0.1.tgz";
      path = fetchurl {
        name = "cross-spawn-3.0.1.tgz";
        url  = "https://registry.yarnpkg.com/cross-spawn/-/cross-spawn-3.0.1.tgz";
        sha1 = "1256037ecb9f0c5f79e3d6ef135e30770184b982";
      };
    }

    {
      name = "crypto-browserify-3.12.0.tgz";
      path = fetchurl {
        name = "crypto-browserify-3.12.0.tgz";
        url  = "https://registry.yarnpkg.com/crypto-browserify/-/crypto-browserify-3.12.0.tgz";
        sha1 = "396cf9f3137f03e4b8e532c58f698254e00f80ec";
      };
    }

    {
      name = "css-loader-1.0.1.tgz";
      path = fetchurl {
        name = "css-loader-1.0.1.tgz";
        url  = "https://registry.yarnpkg.com/css-loader/-/css-loader-1.0.1.tgz";
        sha1 = "6885bb5233b35ec47b006057da01cc640b6b79fe";
      };
    }

    {
      name = "css-select-1.2.0.tgz";
      path = fetchurl {
        name = "css-select-1.2.0.tgz";
        url  = "https://registry.yarnpkg.com/css-select/-/css-select-1.2.0.tgz";
        sha1 = "2b3a110539c5355f1cd8d314623e870b121ec858";
      };
    }

    {
      name = "css-selector-tokenizer-0.7.1.tgz";
      path = fetchurl {
        name = "css-selector-tokenizer-0.7.1.tgz";
        url  = "https://registry.yarnpkg.com/css-selector-tokenizer/-/css-selector-tokenizer-0.7.1.tgz";
        sha1 = "a177271a8bca5019172f4f891fc6eed9cbf68d5d";
      };
    }

    {
      name = "css-what-2.1.3.tgz";
      path = fetchurl {
        name = "css-what-2.1.3.tgz";
        url  = "https://registry.yarnpkg.com/css-what/-/css-what-2.1.3.tgz";
        sha1 = "a6d7604573365fe74686c3f311c56513d88285f2";
      };
    }

    {
      name = "cssesc-0.1.0.tgz";
      path = fetchurl {
        name = "cssesc-0.1.0.tgz";
        url  = "https://registry.yarnpkg.com/cssesc/-/cssesc-0.1.0.tgz";
        sha1 = "c814903e45623371a0477b40109aaafbeeaddbb4";
      };
    }

    {
      name = "currently-unhandled-0.4.1.tgz";
      path = fetchurl {
        name = "currently-unhandled-0.4.1.tgz";
        url  = "https://registry.yarnpkg.com/currently-unhandled/-/currently-unhandled-0.4.1.tgz";
        sha1 = "988df33feab191ef799a61369dd76c17adf957ea";
      };
    }

    {
      name = "cyclist-1.0.1.tgz";
      path = fetchurl {
        name = "cyclist-1.0.1.tgz";
        url  = "https://registry.yarnpkg.com/cyclist/-/cyclist-1.0.1.tgz";
        sha1 = "596e9698fd0c80e12038c2b82d6eb1b35b6224d9";
      };
    }

    {
      name = "dargs-5.1.0.tgz";
      path = fetchurl {
        name = "dargs-5.1.0.tgz";
        url  = "https://registry.yarnpkg.com/dargs/-/dargs-5.1.0.tgz";
        sha1 = "ec7ea50c78564cd36c9d5ec18f66329fade27829";
      };
    }

    {
      name = "dashdash-1.14.1.tgz";
      path = fetchurl {
        name = "dashdash-1.14.1.tgz";
        url  = "https://registry.yarnpkg.com/dashdash/-/dashdash-1.14.1.tgz";
        sha1 = "853cfa0f7cbe2fed5de20326b8dd581035f6e2f0";
      };
    }

    {
      name = "debug-2.6.9.tgz";
      path = fetchurl {
        name = "debug-2.6.9.tgz";
        url  = "https://registry.yarnpkg.com/debug/-/debug-2.6.9.tgz";
        sha1 = "5d128515df134ff327e90a4c93f4e077a536341f";
      };
    }

    {
      name = "debug-3.2.6.tgz";
      path = fetchurl {
        name = "debug-3.2.6.tgz";
        url  = "https://registry.yarnpkg.com/debug/-/debug-3.2.6.tgz";
        sha1 = "e83d17de16d8a7efb7717edbe5fb10135eee629b";
      };
    }

    {
      name = "debug-4.1.1.tgz";
      path = fetchurl {
        name = "debug-4.1.1.tgz";
        url  = "https://registry.yarnpkg.com/debug/-/debug-4.1.1.tgz";
        sha1 = "3b72260255109c6b589cee050f1d516139664791";
      };
    }

    {
      name = "decamelize-1.2.0.tgz";
      path = fetchurl {
        name = "decamelize-1.2.0.tgz";
        url  = "https://registry.yarnpkg.com/decamelize/-/decamelize-1.2.0.tgz";
        sha1 = "f6534d15148269b20352e7bee26f501f9a191290";
      };
    }

    {
      name = "decode-uri-component-0.2.0.tgz";
      path = fetchurl {
        name = "decode-uri-component-0.2.0.tgz";
        url  = "https://registry.yarnpkg.com/decode-uri-component/-/decode-uri-component-0.2.0.tgz";
        sha1 = "eb3913333458775cb84cd1a1fae062106bb87545";
      };
    }

    {
      name = "deep-equal-1.1.1.tgz";
      path = fetchurl {
        name = "deep-equal-1.1.1.tgz";
        url  = "https://registry.yarnpkg.com/deep-equal/-/deep-equal-1.1.1.tgz";
        sha1 = "b5c98c942ceffaf7cb051e24e1434a25a2e6076a";
      };
    }

    {
      name = "default-gateway-4.2.0.tgz";
      path = fetchurl {
        name = "default-gateway-4.2.0.tgz";
        url  = "https://registry.yarnpkg.com/default-gateway/-/default-gateway-4.2.0.tgz";
        sha1 = "167104c7500c2115f6dd69b0a536bb8ed720552b";
      };
    }

    {
      name = "define-properties-1.1.3.tgz";
      path = fetchurl {
        name = "define-properties-1.1.3.tgz";
        url  = "https://registry.yarnpkg.com/define-properties/-/define-properties-1.1.3.tgz";
        sha1 = "cf88da6cbee26fe6db7094f61d870cbd84cee9f1";
      };
    }

    {
      name = "define-property-0.2.5.tgz";
      path = fetchurl {
        name = "define-property-0.2.5.tgz";
        url  = "https://registry.yarnpkg.com/define-property/-/define-property-0.2.5.tgz";
        sha1 = "c35b1ef918ec3c990f9a5bc57be04aacec5c8116";
      };
    }

    {
      name = "define-property-1.0.0.tgz";
      path = fetchurl {
        name = "define-property-1.0.0.tgz";
        url  = "https://registry.yarnpkg.com/define-property/-/define-property-1.0.0.tgz";
        sha1 = "769ebaaf3f4a63aad3af9e8d304c9bbe79bfb0e6";
      };
    }

    {
      name = "define-property-2.0.2.tgz";
      path = fetchurl {
        name = "define-property-2.0.2.tgz";
        url  = "https://registry.yarnpkg.com/define-property/-/define-property-2.0.2.tgz";
        sha1 = "d459689e8d654ba77e02a817f8710d702cb16e9d";
      };
    }

    {
      name = "del-4.1.1.tgz";
      path = fetchurl {
        name = "del-4.1.1.tgz";
        url  = "https://registry.yarnpkg.com/del/-/del-4.1.1.tgz";
        sha1 = "9e8f117222ea44a31ff3a156c049b99052a9f0b4";
      };
    }

    {
      name = "delayed-stream-1.0.0.tgz";
      path = fetchurl {
        name = "delayed-stream-1.0.0.tgz";
        url  = "https://registry.yarnpkg.com/delayed-stream/-/delayed-stream-1.0.0.tgz";
        sha1 = "df3ae199acadfb7d440aaae0b29e2272b24ec619";
      };
    }

    {
      name = "delegates-1.0.0.tgz";
      path = fetchurl {
        name = "delegates-1.0.0.tgz";
        url  = "https://registry.yarnpkg.com/delegates/-/delegates-1.0.0.tgz";
        sha1 = "84c6e159b81904fdca59a0ef44cd870d31250f9a";
      };
    }

    {
      name = "depd-1.1.2.tgz";
      path = fetchurl {
        name = "depd-1.1.2.tgz";
        url  = "https://registry.yarnpkg.com/depd/-/depd-1.1.2.tgz";
        sha1 = "9bcd52e14c097763e749b274c4346ed2e560b5a9";
      };
    }

    {
      name = "des.js-1.0.1.tgz";
      path = fetchurl {
        name = "des.js-1.0.1.tgz";
        url  = "https://registry.yarnpkg.com/des.js/-/des.js-1.0.1.tgz";
        sha1 = "5382142e1bdc53f85d86d53e5f4aa7deb91e0843";
      };
    }

    {
      name = "destroy-1.0.4.tgz";
      path = fetchurl {
        name = "destroy-1.0.4.tgz";
        url  = "https://registry.yarnpkg.com/destroy/-/destroy-1.0.4.tgz";
        sha1 = "978857442c44749e4206613e37946205826abd80";
      };
    }

    {
      name = "detect-file-1.0.0.tgz";
      path = fetchurl {
        name = "detect-file-1.0.0.tgz";
        url  = "https://registry.yarnpkg.com/detect-file/-/detect-file-1.0.0.tgz";
        sha1 = "f0d66d03672a825cb1b73bdb3fe62310c8e552b7";
      };
    }

    {
      name = "detect-node-2.0.4.tgz";
      path = fetchurl {
        name = "detect-node-2.0.4.tgz";
        url  = "https://registry.yarnpkg.com/detect-node/-/detect-node-2.0.4.tgz";
        sha1 = "014ee8f8f669c5c58023da64b8179c083a28c46c";
      };
    }

    {
      name = "diffie-hellman-5.0.3.tgz";
      path = fetchurl {
        name = "diffie-hellman-5.0.3.tgz";
        url  = "https://registry.yarnpkg.com/diffie-hellman/-/diffie-hellman-5.0.3.tgz";
        sha1 = "40e8ee98f55a2149607146921c63e1ae5f3d2875";
      };
    }

    {
      name = "dns-equal-1.0.0.tgz";
      path = fetchurl {
        name = "dns-equal-1.0.0.tgz";
        url  = "https://registry.yarnpkg.com/dns-equal/-/dns-equal-1.0.0.tgz";
        sha1 = "b39e7f1da6eb0a75ba9c17324b34753c47e0654d";
      };
    }

    {
      name = "dns-packet-1.3.1.tgz";
      path = fetchurl {
        name = "dns-packet-1.3.1.tgz";
        url  = "https://registry.yarnpkg.com/dns-packet/-/dns-packet-1.3.1.tgz";
        sha1 = "12aa426981075be500b910eedcd0b47dd7deda5a";
      };
    }

    {
      name = "dns-txt-2.0.2.tgz";
      path = fetchurl {
        name = "dns-txt-2.0.2.tgz";
        url  = "https://registry.yarnpkg.com/dns-txt/-/dns-txt-2.0.2.tgz";
        sha1 = "b91d806f5d27188e4ab3e7d107d881a1cc4642b6";
      };
    }

    {
      name = "dom-converter-0.2.0.tgz";
      path = fetchurl {
        name = "dom-converter-0.2.0.tgz";
        url  = "https://registry.yarnpkg.com/dom-converter/-/dom-converter-0.2.0.tgz";
        sha1 = "6721a9daee2e293682955b6afe416771627bb768";
      };
    }

    {
      name = "dom-serializer-0.2.2.tgz";
      path = fetchurl {
        name = "dom-serializer-0.2.2.tgz";
        url  = "https://registry.yarnpkg.com/dom-serializer/-/dom-serializer-0.2.2.tgz";
        sha1 = "1afb81f533717175d478655debc5e332d9f9bb51";
      };
    }

    {
      name = "domain-browser-1.2.0.tgz";
      path = fetchurl {
        name = "domain-browser-1.2.0.tgz";
        url  = "https://registry.yarnpkg.com/domain-browser/-/domain-browser-1.2.0.tgz";
        sha1 = "3d31f50191a6749dd1375a7f522e823d42e54eda";
      };
    }

    {
      name = "domelementtype-1.3.1.tgz";
      path = fetchurl {
        name = "domelementtype-1.3.1.tgz";
        url  = "https://registry.yarnpkg.com/domelementtype/-/domelementtype-1.3.1.tgz";
        sha1 = "d048c44b37b0d10a7f2a3d5fee3f4333d790481f";
      };
    }

    {
      name = "domelementtype-2.0.1.tgz";
      path = fetchurl {
        name = "domelementtype-2.0.1.tgz";
        url  = "https://registry.yarnpkg.com/domelementtype/-/domelementtype-2.0.1.tgz";
        sha1 = "1f8bdfe91f5a78063274e803b4bdcedf6e94f94d";
      };
    }

    {
      name = "domhandler-2.4.2.tgz";
      path = fetchurl {
        name = "domhandler-2.4.2.tgz";
        url  = "https://registry.yarnpkg.com/domhandler/-/domhandler-2.4.2.tgz";
        sha1 = "8805097e933d65e85546f726d60f5eb88b44f803";
      };
    }

    {
      name = "domutils-1.5.1.tgz";
      path = fetchurl {
        name = "domutils-1.5.1.tgz";
        url  = "https://registry.yarnpkg.com/domutils/-/domutils-1.5.1.tgz";
        sha1 = "dcd8488a26f563d61079e48c9f7b7e32373682cf";
      };
    }

    {
      name = "domutils-1.7.0.tgz";
      path = fetchurl {
        name = "domutils-1.7.0.tgz";
        url  = "https://registry.yarnpkg.com/domutils/-/domutils-1.7.0.tgz";
        sha1 = "56ea341e834e06e6748af7a1cb25da67ea9f8c2a";
      };
    }

    {
      name = "duplexify-3.7.1.tgz";
      path = fetchurl {
        name = "duplexify-3.7.1.tgz";
        url  = "https://registry.yarnpkg.com/duplexify/-/duplexify-3.7.1.tgz";
        sha1 = "2a4df5317f6ccfd91f86d6fd25d8d8a103b88309";
      };
    }

    {
      name = "ecc-jsbn-0.1.2.tgz";
      path = fetchurl {
        name = "ecc-jsbn-0.1.2.tgz";
        url  = "https://registry.yarnpkg.com/ecc-jsbn/-/ecc-jsbn-0.1.2.tgz";
        sha1 = "3a83a904e54353287874c564b7549386849a98c9";
      };
    }

    {
      name = "echarts-4.6.0.tgz";
      path = fetchurl {
        name = "echarts-4.6.0.tgz";
        url  = "https://registry.yarnpkg.com/echarts/-/echarts-4.6.0.tgz";
        sha1 = "b5a47a1046cec93ceeef954f9ee54751340558ec";
      };
    }

    {
      name = "ee-first-1.1.1.tgz";
      path = fetchurl {
        name = "ee-first-1.1.1.tgz";
        url  = "https://registry.yarnpkg.com/ee-first/-/ee-first-1.1.1.tgz";
        sha1 = "590c61156b0ae2f4f0255732a158b266bc56b21d";
      };
    }

    {
      name = "elliptic-6.5.2.tgz";
      path = fetchurl {
        name = "elliptic-6.5.2.tgz";
        url  = "https://registry.yarnpkg.com/elliptic/-/elliptic-6.5.2.tgz";
        sha1 = "05c5678d7173c049d8ca433552224a495d0e3762";
      };
    }

    {
      name = "emoji-regex-7.0.3.tgz";
      path = fetchurl {
        name = "emoji-regex-7.0.3.tgz";
        url  = "https://registry.yarnpkg.com/emoji-regex/-/emoji-regex-7.0.3.tgz";
        sha1 = "933a04052860c85e83c122479c4748a8e4c72156";
      };
    }

    {
      name = "emojis-list-2.1.0.tgz";
      path = fetchurl {
        name = "emojis-list-2.1.0.tgz";
        url  = "https://registry.yarnpkg.com/emojis-list/-/emojis-list-2.1.0.tgz";
        sha1 = "4daa4d9db00f9819880c79fa457ae5b09a1fd389";
      };
    }

    {
      name = "encodeurl-1.0.2.tgz";
      path = fetchurl {
        name = "encodeurl-1.0.2.tgz";
        url  = "https://registry.yarnpkg.com/encodeurl/-/encodeurl-1.0.2.tgz";
        sha1 = "ad3ff4c86ec2d029322f5a02c3a9a606c95b3f59";
      };
    }

    {
      name = "end-of-stream-1.4.4.tgz";
      path = fetchurl {
        name = "end-of-stream-1.4.4.tgz";
        url  = "https://registry.yarnpkg.com/end-of-stream/-/end-of-stream-1.4.4.tgz";
        sha1 = "5ae64a5f45057baf3626ec14da0ca5e4b2431eb0";
      };
    }

    {
      name = "enhanced-resolve-4.1.0.tgz";
      path = fetchurl {
        name = "enhanced-resolve-4.1.0.tgz";
        url  = "https://registry.yarnpkg.com/enhanced-resolve/-/enhanced-resolve-4.1.0.tgz";
        sha1 = "41c7e0bfdfe74ac1ffe1e57ad6a5c6c9f3742a7f";
      };
    }

    {
      name = "enhanced-resolve-4.1.1.tgz";
      path = fetchurl {
        name = "enhanced-resolve-4.1.1.tgz";
        url  = "https://registry.yarnpkg.com/enhanced-resolve/-/enhanced-resolve-4.1.1.tgz";
        sha1 = "2937e2b8066cd0fe7ce0990a98f0d71a35189f66";
      };
    }

    {
      name = "entities-1.1.2.tgz";
      path = fetchurl {
        name = "entities-1.1.2.tgz";
        url  = "https://registry.yarnpkg.com/entities/-/entities-1.1.2.tgz";
        sha1 = "bdfa735299664dfafd34529ed4f8522a275fea56";
      };
    }

    {
      name = "entities-2.0.0.tgz";
      path = fetchurl {
        name = "entities-2.0.0.tgz";
        url  = "https://registry.yarnpkg.com/entities/-/entities-2.0.0.tgz";
        sha1 = "68d6084cab1b079767540d80e56a39b423e4abf4";
      };
    }

    {
      name = "err-code-1.1.2.tgz";
      path = fetchurl {
        name = "err-code-1.1.2.tgz";
        url  = "https://registry.yarnpkg.com/err-code/-/err-code-1.1.2.tgz";
        sha1 = "06e0116d3028f6aef4806849eb0ea6a748ae6960";
      };
    }

    {
      name = "errno-0.1.7.tgz";
      path = fetchurl {
        name = "errno-0.1.7.tgz";
        url  = "https://registry.yarnpkg.com/errno/-/errno-0.1.7.tgz";
        sha1 = "4684d71779ad39af177e3f007996f7c67c852618";
      };
    }

    {
      name = "error-ex-1.3.2.tgz";
      path = fetchurl {
        name = "error-ex-1.3.2.tgz";
        url  = "https://registry.yarnpkg.com/error-ex/-/error-ex-1.3.2.tgz";
        sha1 = "b4ac40648107fdcdcfae242f428bea8a14d4f1bf";
      };
    }

    {
      name = "es-abstract-1.17.4.tgz";
      path = fetchurl {
        name = "es-abstract-1.17.4.tgz";
        url  = "https://registry.yarnpkg.com/es-abstract/-/es-abstract-1.17.4.tgz";
        sha1 = "e3aedf19706b20e7c2594c35fc0d57605a79e184";
      };
    }

    {
      name = "es-to-primitive-1.2.1.tgz";
      path = fetchurl {
        name = "es-to-primitive-1.2.1.tgz";
        url  = "https://registry.yarnpkg.com/es-to-primitive/-/es-to-primitive-1.2.1.tgz";
        sha1 = "e55cd4c9cdc188bcefb03b366c736323fc5c898a";
      };
    }

    {
      name = "escape-html-1.0.3.tgz";
      path = fetchurl {
        name = "escape-html-1.0.3.tgz";
        url  = "https://registry.yarnpkg.com/escape-html/-/escape-html-1.0.3.tgz";
        sha1 = "0258eae4d3d0c0974de1c169188ef0051d1d1988";
      };
    }

    {
      name = "escape-string-regexp-1.0.5.tgz";
      path = fetchurl {
        name = "escape-string-regexp-1.0.5.tgz";
        url  = "https://registry.yarnpkg.com/escape-string-regexp/-/escape-string-regexp-1.0.5.tgz";
        sha1 = "1b61c0562190a8dff6ae3bb2cf0200ca130b86d4";
      };
    }

    {
      name = "eslint-scope-4.0.3.tgz";
      path = fetchurl {
        name = "eslint-scope-4.0.3.tgz";
        url  = "https://registry.yarnpkg.com/eslint-scope/-/eslint-scope-4.0.3.tgz";
        sha1 = "ca03833310f6889a3264781aa82e63eb9cfe7848";
      };
    }

    {
      name = "esrecurse-4.2.1.tgz";
      path = fetchurl {
        name = "esrecurse-4.2.1.tgz";
        url  = "https://registry.yarnpkg.com/esrecurse/-/esrecurse-4.2.1.tgz";
        sha1 = "007a3b9fdbc2b3bb87e4879ea19c92fdbd3942cf";
      };
    }

    {
      name = "estraverse-4.3.0.tgz";
      path = fetchurl {
        name = "estraverse-4.3.0.tgz";
        url  = "https://registry.yarnpkg.com/estraverse/-/estraverse-4.3.0.tgz";
        sha1 = "398ad3f3c5a24948be7725e83d11a7de28cdbd1d";
      };
    }

    {
      name = "esutils-2.0.3.tgz";
      path = fetchurl {
        name = "esutils-2.0.3.tgz";
        url  = "https://registry.yarnpkg.com/esutils/-/esutils-2.0.3.tgz";
        sha1 = "74d2eb4de0b8da1293711910d50775b9b710ef64";
      };
    }

    {
      name = "etag-1.8.1.tgz";
      path = fetchurl {
        name = "etag-1.8.1.tgz";
        url  = "https://registry.yarnpkg.com/etag/-/etag-1.8.1.tgz";
        sha1 = "41ae2eeb65efa62268aebfea83ac7d79299b0887";
      };
    }

    {
      name = "eventemitter3-4.0.0.tgz";
      path = fetchurl {
        name = "eventemitter3-4.0.0.tgz";
        url  = "https://registry.yarnpkg.com/eventemitter3/-/eventemitter3-4.0.0.tgz";
        sha1 = "d65176163887ee59f386d64c82610b696a4a74eb";
      };
    }

    {
      name = "events-3.1.0.tgz";
      path = fetchurl {
        name = "events-3.1.0.tgz";
        url  = "https://registry.yarnpkg.com/events/-/events-3.1.0.tgz";
        sha1 = "84279af1b34cb75aa88bf5ff291f6d0bd9b31a59";
      };
    }

    {
      name = "eventsource-1.0.7.tgz";
      path = fetchurl {
        name = "eventsource-1.0.7.tgz";
        url  = "https://registry.yarnpkg.com/eventsource/-/eventsource-1.0.7.tgz";
        sha1 = "8fbc72c93fcd34088090bc0a4e64f4b5cee6d8d0";
      };
    }

    {
      name = "evp_bytestokey-1.0.3.tgz";
      path = fetchurl {
        name = "evp_bytestokey-1.0.3.tgz";
        url  = "https://registry.yarnpkg.com/evp_bytestokey/-/evp_bytestokey-1.0.3.tgz";
        sha1 = "7fcbdb198dc71959432efe13842684e0525acb02";
      };
    }

    {
      name = "execa-1.0.0.tgz";
      path = fetchurl {
        name = "execa-1.0.0.tgz";
        url  = "https://registry.yarnpkg.com/execa/-/execa-1.0.0.tgz";
        sha1 = "c6236a5bb4df6d6f15e88e7f017798216749ddd8";
      };
    }

    {
      name = "expand-brackets-2.1.4.tgz";
      path = fetchurl {
        name = "expand-brackets-2.1.4.tgz";
        url  = "https://registry.yarnpkg.com/expand-brackets/-/expand-brackets-2.1.4.tgz";
        sha1 = "b77735e315ce30f6b6eff0f83b04151a22449622";
      };
    }

    {
      name = "expand-tilde-2.0.2.tgz";
      path = fetchurl {
        name = "expand-tilde-2.0.2.tgz";
        url  = "https://registry.yarnpkg.com/expand-tilde/-/expand-tilde-2.0.2.tgz";
        sha1 = "97e801aa052df02454de46b02bf621642cdc8502";
      };
    }

    {
      name = "express-4.17.1.tgz";
      path = fetchurl {
        name = "express-4.17.1.tgz";
        url  = "https://registry.yarnpkg.com/express/-/express-4.17.1.tgz";
        sha1 = "4491fc38605cf51f8629d39c2b5d026f98a4c134";
      };
    }

    {
      name = "extend-shallow-2.0.1.tgz";
      path = fetchurl {
        name = "extend-shallow-2.0.1.tgz";
        url  = "https://registry.yarnpkg.com/extend-shallow/-/extend-shallow-2.0.1.tgz";
        sha1 = "51af7d614ad9a9f610ea1bafbb989d6b1c56890f";
      };
    }

    {
      name = "extend-shallow-3.0.2.tgz";
      path = fetchurl {
        name = "extend-shallow-3.0.2.tgz";
        url  = "https://registry.yarnpkg.com/extend-shallow/-/extend-shallow-3.0.2.tgz";
        sha1 = "26a71aaf073b39fb2127172746131c2704028db8";
      };
    }

    {
      name = "extend-3.0.2.tgz";
      path = fetchurl {
        name = "extend-3.0.2.tgz";
        url  = "https://registry.yarnpkg.com/extend/-/extend-3.0.2.tgz";
        sha1 = "f8b1136b4071fbd8eb140aff858b1019ec2915fa";
      };
    }

    {
      name = "extglob-2.0.4.tgz";
      path = fetchurl {
        name = "extglob-2.0.4.tgz";
        url  = "https://registry.yarnpkg.com/extglob/-/extglob-2.0.4.tgz";
        sha1 = "ad00fe4dc612a9232e8718711dc5cb5ab0285543";
      };
    }

    {
      name = "extract-text-webpack-plugin-3.0.2.tgz";
      path = fetchurl {
        name = "extract-text-webpack-plugin-3.0.2.tgz";
        url  = "https://registry.yarnpkg.com/extract-text-webpack-plugin/-/extract-text-webpack-plugin-3.0.2.tgz";
        sha1 = "5f043eaa02f9750a9258b78c0a6e0dc1408fb2f7";
      };
    }

    {
      name = "extsprintf-1.3.0.tgz";
      path = fetchurl {
        name = "extsprintf-1.3.0.tgz";
        url  = "https://registry.yarnpkg.com/extsprintf/-/extsprintf-1.3.0.tgz";
        sha1 = "96918440e3041a7a414f8c52e3c574eb3c3e1e05";
      };
    }

    {
      name = "extsprintf-1.4.0.tgz";
      path = fetchurl {
        name = "extsprintf-1.4.0.tgz";
        url  = "https://registry.yarnpkg.com/extsprintf/-/extsprintf-1.4.0.tgz";
        sha1 = "e2689f8f356fad62cca65a3a91c5df5f9551692f";
      };
    }

    {
      name = "fast-deep-equal-1.1.0.tgz";
      path = fetchurl {
        name = "fast-deep-equal-1.1.0.tgz";
        url  = "https://registry.yarnpkg.com/fast-deep-equal/-/fast-deep-equal-1.1.0.tgz";
        sha1 = "c053477817c86b51daa853c81e059b733d023614";
      };
    }

    {
      name = "fast-deep-equal-3.1.1.tgz";
      path = fetchurl {
        name = "fast-deep-equal-3.1.1.tgz";
        url  = "https://registry.yarnpkg.com/fast-deep-equal/-/fast-deep-equal-3.1.1.tgz";
        sha1 = "545145077c501491e33b15ec408c294376e94ae4";
      };
    }

    {
      name = "fast-json-stable-stringify-2.1.0.tgz";
      path = fetchurl {
        name = "fast-json-stable-stringify-2.1.0.tgz";
        url  = "https://registry.yarnpkg.com/fast-json-stable-stringify/-/fast-json-stable-stringify-2.1.0.tgz";
        sha1 = "874bf69c6f404c2b5d99c481341399fd55892633";
      };
    }

    {
      name = "fastparse-1.1.2.tgz";
      path = fetchurl {
        name = "fastparse-1.1.2.tgz";
        url  = "https://registry.yarnpkg.com/fastparse/-/fastparse-1.1.2.tgz";
        sha1 = "91728c5a5942eced8531283c79441ee4122c35a9";
      };
    }

    {
      name = "faye-websocket-0.10.0.tgz";
      path = fetchurl {
        name = "faye-websocket-0.10.0.tgz";
        url  = "https://registry.yarnpkg.com/faye-websocket/-/faye-websocket-0.10.0.tgz";
        sha1 = "4e492f8d04dfb6f89003507f6edbf2d501e7c6f4";
      };
    }

    {
      name = "faye-websocket-0.11.3.tgz";
      path = fetchurl {
        name = "faye-websocket-0.11.3.tgz";
        url  = "https://registry.yarnpkg.com/faye-websocket/-/faye-websocket-0.11.3.tgz";
        sha1 = "5c0e9a8968e8912c286639fde977a8b209f2508e";
      };
    }

    {
      name = "figgy-pudding-3.5.1.tgz";
      path = fetchurl {
        name = "figgy-pudding-3.5.1.tgz";
        url  = "https://registry.yarnpkg.com/figgy-pudding/-/figgy-pudding-3.5.1.tgz";
        sha1 = "862470112901c727a0e495a80744bd5baa1d6790";
      };
    }

    {
      name = "file-loader-2.0.0.tgz";
      path = fetchurl {
        name = "file-loader-2.0.0.tgz";
        url  = "https://registry.yarnpkg.com/file-loader/-/file-loader-2.0.0.tgz";
        sha1 = "39749c82f020b9e85901dcff98e8004e6401cfde";
      };
    }

    {
      name = "file-uri-to-path-1.0.0.tgz";
      path = fetchurl {
        name = "file-uri-to-path-1.0.0.tgz";
        url  = "https://registry.yarnpkg.com/file-uri-to-path/-/file-uri-to-path-1.0.0.tgz";
        sha1 = "553a7b8446ff6f684359c445f1e37a05dacc33dd";
      };
    }

    {
      name = "fill-range-4.0.0.tgz";
      path = fetchurl {
        name = "fill-range-4.0.0.tgz";
        url  = "https://registry.yarnpkg.com/fill-range/-/fill-range-4.0.0.tgz";
        sha1 = "d544811d428f98eb06a63dc402d2403c328c38f7";
      };
    }

    {
      name = "finalhandler-1.1.2.tgz";
      path = fetchurl {
        name = "finalhandler-1.1.2.tgz";
        url  = "https://registry.yarnpkg.com/finalhandler/-/finalhandler-1.1.2.tgz";
        sha1 = "b7e7d000ffd11938d0fdb053506f6ebabe9f587d";
      };
    }

    {
      name = "find-cache-dir-2.1.0.tgz";
      path = fetchurl {
        name = "find-cache-dir-2.1.0.tgz";
        url  = "https://registry.yarnpkg.com/find-cache-dir/-/find-cache-dir-2.1.0.tgz";
        sha1 = "8d0f94cd13fe43c6c7c261a0d86115ca918c05f7";
      };
    }

    {
      name = "find-up-1.1.2.tgz";
      path = fetchurl {
        name = "find-up-1.1.2.tgz";
        url  = "https://registry.yarnpkg.com/find-up/-/find-up-1.1.2.tgz";
        sha1 = "6b2e9822b1a2ce0a60ab64d610eccad53cb24d0f";
      };
    }

    {
      name = "find-up-3.0.0.tgz";
      path = fetchurl {
        name = "find-up-3.0.0.tgz";
        url  = "https://registry.yarnpkg.com/find-up/-/find-up-3.0.0.tgz";
        sha1 = "49169f1d7993430646da61ecc5ae355c21c97b73";
      };
    }

    {
      name = "findup-sync-3.0.0.tgz";
      path = fetchurl {
        name = "findup-sync-3.0.0.tgz";
        url  = "https://registry.yarnpkg.com/findup-sync/-/findup-sync-3.0.0.tgz";
        sha1 = "17b108f9ee512dfb7a5c7f3c8b27ea9e1a9c08d1";
      };
    }

    {
      name = "flush-write-stream-1.1.1.tgz";
      path = fetchurl {
        name = "flush-write-stream-1.1.1.tgz";
        url  = "https://registry.yarnpkg.com/flush-write-stream/-/flush-write-stream-1.1.1.tgz";
        sha1 = "8dd7d873a1babc207d94ead0c2e0e44276ebf2e8";
      };
    }

    {
      name = "follow-redirects-1.10.0.tgz";
      path = fetchurl {
        name = "follow-redirects-1.10.0.tgz";
        url  = "https://registry.yarnpkg.com/follow-redirects/-/follow-redirects-1.10.0.tgz";
        sha1 = "01f5263aee921c6a54fb91667f08f4155ce169eb";
      };
    }

    {
      name = "for-in-1.0.2.tgz";
      path = fetchurl {
        name = "for-in-1.0.2.tgz";
        url  = "https://registry.yarnpkg.com/for-in/-/for-in-1.0.2.tgz";
        sha1 = "81068d295a8142ec0ac726c6e2200c30fb6d5e80";
      };
    }

    {
      name = "forever-agent-0.6.1.tgz";
      path = fetchurl {
        name = "forever-agent-0.6.1.tgz";
        url  = "https://registry.yarnpkg.com/forever-agent/-/forever-agent-0.6.1.tgz";
        sha1 = "fbc71f0c41adeb37f96c577ad1ed42d8fdacca91";
      };
    }

    {
      name = "form-data-2.3.3.tgz";
      path = fetchurl {
        name = "form-data-2.3.3.tgz";
        url  = "https://registry.yarnpkg.com/form-data/-/form-data-2.3.3.tgz";
        sha1 = "dcce52c05f644f298c6a7ab936bd724ceffbf3a6";
      };
    }

    {
      name = "forwarded-0.1.2.tgz";
      path = fetchurl {
        name = "forwarded-0.1.2.tgz";
        url  = "https://registry.yarnpkg.com/forwarded/-/forwarded-0.1.2.tgz";
        sha1 = "98c23dab1175657b8c0573e8ceccd91b0ff18c84";
      };
    }

    {
      name = "fragment-cache-0.2.1.tgz";
      path = fetchurl {
        name = "fragment-cache-0.2.1.tgz";
        url  = "https://registry.yarnpkg.com/fragment-cache/-/fragment-cache-0.2.1.tgz";
        sha1 = "4290fad27f13e89be7f33799c6bc5a0abfff0d19";
      };
    }

    {
      name = "fresh-0.5.2.tgz";
      path = fetchurl {
        name = "fresh-0.5.2.tgz";
        url  = "https://registry.yarnpkg.com/fresh/-/fresh-0.5.2.tgz";
        sha1 = "3d8cadd90d976569fa835ab1f8e4b23a105605a7";
      };
    }

    {
      name = "from2-2.3.0.tgz";
      path = fetchurl {
        name = "from2-2.3.0.tgz";
        url  = "https://registry.yarnpkg.com/from2/-/from2-2.3.0.tgz";
        sha1 = "8bfb5502bde4a4d36cfdeea007fcca21d7e382af";
      };
    }

    {
      name = "fs-write-stream-atomic-1.0.10.tgz";
      path = fetchurl {
        name = "fs-write-stream-atomic-1.0.10.tgz";
        url  = "https://registry.yarnpkg.com/fs-write-stream-atomic/-/fs-write-stream-atomic-1.0.10.tgz";
        sha1 = "b47df53493ef911df75731e70a9ded0189db40c9";
      };
    }

    {
      name = "fs.realpath-1.0.0.tgz";
      path = fetchurl {
        name = "fs.realpath-1.0.0.tgz";
        url  = "https://registry.yarnpkg.com/fs.realpath/-/fs.realpath-1.0.0.tgz";
        sha1 = "1504ad2523158caa40db4a2787cb01411994ea4f";
      };
    }

    {
      name = "fsevents-1.2.11.tgz";
      path = fetchurl {
        name = "fsevents-1.2.11.tgz";
        url  = "https://registry.yarnpkg.com/fsevents/-/fsevents-1.2.11.tgz";
        sha1 = "67bf57f4758f02ede88fb2a1712fef4d15358be3";
      };
    }

    {
      name = "fstream-1.0.12.tgz";
      path = fetchurl {
        name = "fstream-1.0.12.tgz";
        url  = "https://registry.yarnpkg.com/fstream/-/fstream-1.0.12.tgz";
        sha1 = "4e8ba8ee2d48be4f7d0de505455548eae5932045";
      };
    }

    {
      name = "function-bind-1.1.1.tgz";
      path = fetchurl {
        name = "function-bind-1.1.1.tgz";
        url  = "https://registry.yarnpkg.com/function-bind/-/function-bind-1.1.1.tgz";
        sha1 = "a56899d3ea3c9bab874bb9773b7c5ede92f4895d";
      };
    }

    {
      name = "gauge-2.7.4.tgz";
      path = fetchurl {
        name = "gauge-2.7.4.tgz";
        url  = "https://registry.yarnpkg.com/gauge/-/gauge-2.7.4.tgz";
        sha1 = "2c03405c7538c39d7eb37b317022e325fb018bf7";
      };
    }

    {
      name = "gaze-1.1.3.tgz";
      path = fetchurl {
        name = "gaze-1.1.3.tgz";
        url  = "https://registry.yarnpkg.com/gaze/-/gaze-1.1.3.tgz";
        sha1 = "c441733e13b927ac8c0ff0b4c3b033f28812924a";
      };
    }

    {
      name = "get-caller-file-1.0.3.tgz";
      path = fetchurl {
        name = "get-caller-file-1.0.3.tgz";
        url  = "https://registry.yarnpkg.com/get-caller-file/-/get-caller-file-1.0.3.tgz";
        sha1 = "f978fa4c90d1dfe7ff2d6beda2a515e713bdcf4a";
      };
    }

    {
      name = "get-caller-file-2.0.5.tgz";
      path = fetchurl {
        name = "get-caller-file-2.0.5.tgz";
        url  = "https://registry.yarnpkg.com/get-caller-file/-/get-caller-file-2.0.5.tgz";
        sha1 = "4f94412a82db32f36e3b0b9741f8a97feb031f7e";
      };
    }

    {
      name = "get-stdin-4.0.1.tgz";
      path = fetchurl {
        name = "get-stdin-4.0.1.tgz";
        url  = "https://registry.yarnpkg.com/get-stdin/-/get-stdin-4.0.1.tgz";
        sha1 = "b968c6b0a04384324902e8bf1a5df32579a450fe";
      };
    }

    {
      name = "get-stream-4.1.0.tgz";
      path = fetchurl {
        name = "get-stream-4.1.0.tgz";
        url  = "https://registry.yarnpkg.com/get-stream/-/get-stream-4.1.0.tgz";
        sha1 = "c1b255575f3dc21d59bfc79cd3d2b46b1c3a54b5";
      };
    }

    {
      name = "get-value-2.0.6.tgz";
      path = fetchurl {
        name = "get-value-2.0.6.tgz";
        url  = "https://registry.yarnpkg.com/get-value/-/get-value-2.0.6.tgz";
        sha1 = "dc15ca1c672387ca76bd37ac0a395ba2042a2c28";
      };
    }

    {
      name = "getpass-0.1.7.tgz";
      path = fetchurl {
        name = "getpass-0.1.7.tgz";
        url  = "https://registry.yarnpkg.com/getpass/-/getpass-0.1.7.tgz";
        sha1 = "5eff8e3e684d569ae4cb2b1282604e8ba62149fa";
      };
    }

    {
      name = "glob-parent-3.1.0.tgz";
      path = fetchurl {
        name = "glob-parent-3.1.0.tgz";
        url  = "https://registry.yarnpkg.com/glob-parent/-/glob-parent-3.1.0.tgz";
        sha1 = "9e6af6299d8d3bd2bd40430832bd113df906c5ae";
      };
    }

    {
      name = "glob-6.0.4.tgz";
      path = fetchurl {
        name = "glob-6.0.4.tgz";
        url  = "https://registry.yarnpkg.com/glob/-/glob-6.0.4.tgz";
        sha1 = "0f08860f6a155127b2fadd4f9ce24b1aab6e4d22";
      };
    }

    {
      name = "glob-7.1.6.tgz";
      path = fetchurl {
        name = "glob-7.1.6.tgz";
        url  = "https://registry.yarnpkg.com/glob/-/glob-7.1.6.tgz";
        sha1 = "141f33b81a7c2492e125594307480c46679278a6";
      };
    }

    {
      name = "global-modules-2.0.0.tgz";
      path = fetchurl {
        name = "global-modules-2.0.0.tgz";
        url  = "https://registry.yarnpkg.com/global-modules/-/global-modules-2.0.0.tgz";
        sha1 = "997605ad2345f27f51539bea26574421215c7780";
      };
    }

    {
      name = "global-modules-1.0.0.tgz";
      path = fetchurl {
        name = "global-modules-1.0.0.tgz";
        url  = "https://registry.yarnpkg.com/global-modules/-/global-modules-1.0.0.tgz";
        sha1 = "6d770f0eb523ac78164d72b5e71a8877265cc3ea";
      };
    }

    {
      name = "global-prefix-1.0.2.tgz";
      path = fetchurl {
        name = "global-prefix-1.0.2.tgz";
        url  = "https://registry.yarnpkg.com/global-prefix/-/global-prefix-1.0.2.tgz";
        sha1 = "dbf743c6c14992593c655568cb66ed32c0122ebe";
      };
    }

    {
      name = "global-prefix-3.0.0.tgz";
      path = fetchurl {
        name = "global-prefix-3.0.0.tgz";
        url  = "https://registry.yarnpkg.com/global-prefix/-/global-prefix-3.0.0.tgz";
        sha1 = "fc85f73064df69f50421f47f883fe5b913ba9b97";
      };
    }

    {
      name = "globby-4.1.0.tgz";
      path = fetchurl {
        name = "globby-4.1.0.tgz";
        url  = "https://registry.yarnpkg.com/globby/-/globby-4.1.0.tgz";
        sha1 = "080f54549ec1b82a6c60e631fc82e1211dbe95f8";
      };
    }

    {
      name = "globby-6.1.0.tgz";
      path = fetchurl {
        name = "globby-6.1.0.tgz";
        url  = "https://registry.yarnpkg.com/globby/-/globby-6.1.0.tgz";
        sha1 = "f5a6d70e8395e21c858fb0489d64df02424d506c";
      };
    }

    {
      name = "globule-1.3.1.tgz";
      path = fetchurl {
        name = "globule-1.3.1.tgz";
        url  = "https://registry.yarnpkg.com/globule/-/globule-1.3.1.tgz";
        sha1 = "90a25338f22b7fbeb527cee63c629aea754d33b9";
      };
    }

    {
      name = "graceful-fs-4.2.3.tgz";
      path = fetchurl {
        name = "graceful-fs-4.2.3.tgz";
        url  = "https://registry.yarnpkg.com/graceful-fs/-/graceful-fs-4.2.3.tgz";
        sha1 = "4a12ff1b60376ef09862c2093edd908328be8423";
      };
    }

    {
      name = "graphql-14.6.0.tgz";
      path = fetchurl {
        name = "graphql-14.6.0.tgz";
        url  = "https://registry.yarnpkg.com/graphql/-/graphql-14.6.0.tgz";
        sha1 = "57822297111e874ea12f5cd4419616930cd83e49";
      };
    }

    {
      name = "handle-thing-2.0.0.tgz";
      path = fetchurl {
        name = "handle-thing-2.0.0.tgz";
        url  = "https://registry.yarnpkg.com/handle-thing/-/handle-thing-2.0.0.tgz";
        sha1 = "0e039695ff50c93fc288557d696f3c1dc6776754";
      };
    }

    {
      name = "har-schema-2.0.0.tgz";
      path = fetchurl {
        name = "har-schema-2.0.0.tgz";
        url  = "https://registry.yarnpkg.com/har-schema/-/har-schema-2.0.0.tgz";
        sha1 = "a94c2224ebcac04782a0d9035521f24735b7ec92";
      };
    }

    {
      name = "har-validator-5.1.3.tgz";
      path = fetchurl {
        name = "har-validator-5.1.3.tgz";
        url  = "https://registry.yarnpkg.com/har-validator/-/har-validator-5.1.3.tgz";
        sha1 = "1ef89ebd3e4996557675eed9893110dc350fa080";
      };
    }

    {
      name = "has-ansi-2.0.0.tgz";
      path = fetchurl {
        name = "has-ansi-2.0.0.tgz";
        url  = "https://registry.yarnpkg.com/has-ansi/-/has-ansi-2.0.0.tgz";
        sha1 = "34f5049ce1ecdf2b0649af3ef24e45ed35416d91";
      };
    }

    {
      name = "has-flag-3.0.0.tgz";
      path = fetchurl {
        name = "has-flag-3.0.0.tgz";
        url  = "https://registry.yarnpkg.com/has-flag/-/has-flag-3.0.0.tgz";
        sha1 = "b5d454dc2199ae225699f3467e5a07f3b955bafd";
      };
    }

    {
      name = "has-symbols-1.0.1.tgz";
      path = fetchurl {
        name = "has-symbols-1.0.1.tgz";
        url  = "https://registry.yarnpkg.com/has-symbols/-/has-symbols-1.0.1.tgz";
        sha1 = "9f5214758a44196c406d9bd76cebf81ec2dd31e8";
      };
    }

    {
      name = "has-unicode-2.0.1.tgz";
      path = fetchurl {
        name = "has-unicode-2.0.1.tgz";
        url  = "https://registry.yarnpkg.com/has-unicode/-/has-unicode-2.0.1.tgz";
        sha1 = "e0e6fe6a28cf51138855e086d1691e771de2a8b9";
      };
    }

    {
      name = "has-value-0.3.1.tgz";
      path = fetchurl {
        name = "has-value-0.3.1.tgz";
        url  = "https://registry.yarnpkg.com/has-value/-/has-value-0.3.1.tgz";
        sha1 = "7b1f58bada62ca827ec0a2078025654845995e1f";
      };
    }

    {
      name = "has-value-1.0.0.tgz";
      path = fetchurl {
        name = "has-value-1.0.0.tgz";
        url  = "https://registry.yarnpkg.com/has-value/-/has-value-1.0.0.tgz";
        sha1 = "18b281da585b1c5c51def24c930ed29a0be6b177";
      };
    }

    {
      name = "has-values-0.1.4.tgz";
      path = fetchurl {
        name = "has-values-0.1.4.tgz";
        url  = "https://registry.yarnpkg.com/has-values/-/has-values-0.1.4.tgz";
        sha1 = "6d61de95d91dfca9b9a02089ad384bff8f62b771";
      };
    }

    {
      name = "has-values-1.0.0.tgz";
      path = fetchurl {
        name = "has-values-1.0.0.tgz";
        url  = "https://registry.yarnpkg.com/has-values/-/has-values-1.0.0.tgz";
        sha1 = "95b0b63fec2146619a6fe57fe75628d5a39efe4f";
      };
    }

    {
      name = "has-1.0.3.tgz";
      path = fetchurl {
        name = "has-1.0.3.tgz";
        url  = "https://registry.yarnpkg.com/has/-/has-1.0.3.tgz";
        sha1 = "722d7cbfc1f6aa8241f16dd814e011e1f41e8796";
      };
    }

    {
      name = "hash-base-3.0.4.tgz";
      path = fetchurl {
        name = "hash-base-3.0.4.tgz";
        url  = "https://registry.yarnpkg.com/hash-base/-/hash-base-3.0.4.tgz";
        sha1 = "5fc8686847ecd73499403319a6b0a3f3f6ae4918";
      };
    }

    {
      name = "hash.js-1.1.7.tgz";
      path = fetchurl {
        name = "hash.js-1.1.7.tgz";
        url  = "https://registry.yarnpkg.com/hash.js/-/hash.js-1.1.7.tgz";
        sha1 = "0babca538e8d4ee4a0f8988d68866537a003cf42";
      };
    }

    {
      name = "he-1.2.0.tgz";
      path = fetchurl {
        name = "he-1.2.0.tgz";
        url  = "https://registry.yarnpkg.com/he/-/he-1.2.0.tgz";
        sha1 = "84ae65fa7eafb165fddb61566ae14baf05664f0f";
      };
    }

    {
      name = "hmac-drbg-1.0.1.tgz";
      path = fetchurl {
        name = "hmac-drbg-1.0.1.tgz";
        url  = "https://registry.yarnpkg.com/hmac-drbg/-/hmac-drbg-1.0.1.tgz";
        sha1 = "d2745701025a6c775a6c545793ed502fc0c649a1";
      };
    }

    {
      name = "homedir-polyfill-1.0.3.tgz";
      path = fetchurl {
        name = "homedir-polyfill-1.0.3.tgz";
        url  = "https://registry.yarnpkg.com/homedir-polyfill/-/homedir-polyfill-1.0.3.tgz";
        sha1 = "743298cef4e5af3e194161fbadcc2151d3a058e8";
      };
    }

    {
      name = "hosted-git-info-2.8.5.tgz";
      path = fetchurl {
        name = "hosted-git-info-2.8.5.tgz";
        url  = "https://registry.yarnpkg.com/hosted-git-info/-/hosted-git-info-2.8.5.tgz";
        sha1 = "759cfcf2c4d156ade59b0b2dfabddc42a6b9c70c";
      };
    }

    {
      name = "hpack.js-2.1.6.tgz";
      path = fetchurl {
        name = "hpack.js-2.1.6.tgz";
        url  = "https://registry.yarnpkg.com/hpack.js/-/hpack.js-2.1.6.tgz";
        sha1 = "87774c0949e513f42e84575b3c45681fade2a0b2";
      };
    }

    {
      name = "html-entities-1.2.1.tgz";
      path = fetchurl {
        name = "html-entities-1.2.1.tgz";
        url  = "https://registry.yarnpkg.com/html-entities/-/html-entities-1.2.1.tgz";
        sha1 = "0df29351f0721163515dfb9e5543e5f6eed5162f";
      };
    }

    {
      name = "html-minifier-3.5.21.tgz";
      path = fetchurl {
        name = "html-minifier-3.5.21.tgz";
        url  = "https://registry.yarnpkg.com/html-minifier/-/html-minifier-3.5.21.tgz";
        sha1 = "d0040e054730e354db008463593194015212d20c";
      };
    }

    {
      name = "html-webpack-plugin-3.2.0.tgz";
      path = fetchurl {
        name = "html-webpack-plugin-3.2.0.tgz";
        url  = "https://registry.yarnpkg.com/html-webpack-plugin/-/html-webpack-plugin-3.2.0.tgz";
        sha1 = "b01abbd723acaaa7b37b6af4492ebda03d9dd37b";
      };
    }

    {
      name = "htmlparser2-3.10.1.tgz";
      path = fetchurl {
        name = "htmlparser2-3.10.1.tgz";
        url  = "https://registry.yarnpkg.com/htmlparser2/-/htmlparser2-3.10.1.tgz";
        sha1 = "bd679dc3f59897b6a34bb10749c855bb53a9392f";
      };
    }

    {
      name = "http-deceiver-1.2.7.tgz";
      path = fetchurl {
        name = "http-deceiver-1.2.7.tgz";
        url  = "https://registry.yarnpkg.com/http-deceiver/-/http-deceiver-1.2.7.tgz";
        sha1 = "fa7168944ab9a519d337cb0bec7284dc3e723d87";
      };
    }

    {
      name = "http-errors-1.7.2.tgz";
      path = fetchurl {
        name = "http-errors-1.7.2.tgz";
        url  = "https://registry.yarnpkg.com/http-errors/-/http-errors-1.7.2.tgz";
        sha1 = "4f5029cf13239f31036e5b2e55292bcfbcc85c8f";
      };
    }

    {
      name = "http-errors-1.6.3.tgz";
      path = fetchurl {
        name = "http-errors-1.6.3.tgz";
        url  = "https://registry.yarnpkg.com/http-errors/-/http-errors-1.6.3.tgz";
        sha1 = "8b55680bb4be283a0b5bf4ea2e38580be1d9320d";
      };
    }

    {
      name = "http-errors-1.7.3.tgz";
      path = fetchurl {
        name = "http-errors-1.7.3.tgz";
        url  = "https://registry.yarnpkg.com/http-errors/-/http-errors-1.7.3.tgz";
        sha1 = "6c619e4f9c60308c38519498c14fbb10aacebb06";
      };
    }

    {
      name = "http-parser-js-0.4.10.tgz";
      path = fetchurl {
        name = "http-parser-js-0.4.10.tgz";
        url  = "https://registry.yarnpkg.com/http-parser-js/-/http-parser-js-0.4.10.tgz";
        sha1 = "92c9c1374c35085f75db359ec56cc257cbb93fa4";
      };
    }

    {
      name = "http-proxy-middleware-0.19.1.tgz";
      path = fetchurl {
        name = "http-proxy-middleware-0.19.1.tgz";
        url  = "https://registry.yarnpkg.com/http-proxy-middleware/-/http-proxy-middleware-0.19.1.tgz";
        sha1 = "183c7dc4aa1479150306498c210cdaf96080a43a";
      };
    }

    {
      name = "http-proxy-1.18.0.tgz";
      path = fetchurl {
        name = "http-proxy-1.18.0.tgz";
        url  = "https://registry.yarnpkg.com/http-proxy/-/http-proxy-1.18.0.tgz";
        sha1 = "dbe55f63e75a347db7f3d99974f2692a314a6a3a";
      };
    }

    {
      name = "http-signature-1.2.0.tgz";
      path = fetchurl {
        name = "http-signature-1.2.0.tgz";
        url  = "https://registry.yarnpkg.com/http-signature/-/http-signature-1.2.0.tgz";
        sha1 = "9aecd925114772f3d95b65a60abb8f7c18fbace1";
      };
    }

    {
      name = "https-browserify-1.0.0.tgz";
      path = fetchurl {
        name = "https-browserify-1.0.0.tgz";
        url  = "https://registry.yarnpkg.com/https-browserify/-/https-browserify-1.0.0.tgz";
        sha1 = "ec06c10e0a34c0f2faf199f7fd7fc78fffd03c73";
      };
    }

    {
      name = "iconv-lite-0.4.24.tgz";
      path = fetchurl {
        name = "iconv-lite-0.4.24.tgz";
        url  = "https://registry.yarnpkg.com/iconv-lite/-/iconv-lite-0.4.24.tgz";
        sha1 = "2022b4b25fbddc21d2f524974a474aafe733908b";
      };
    }

    {
      name = "icss-replace-symbols-1.1.0.tgz";
      path = fetchurl {
        name = "icss-replace-symbols-1.1.0.tgz";
        url  = "https://registry.yarnpkg.com/icss-replace-symbols/-/icss-replace-symbols-1.1.0.tgz";
        sha1 = "06ea6f83679a7749e386cfe1fe812ae5db223ded";
      };
    }

    {
      name = "icss-utils-2.1.0.tgz";
      path = fetchurl {
        name = "icss-utils-2.1.0.tgz";
        url  = "https://registry.yarnpkg.com/icss-utils/-/icss-utils-2.1.0.tgz";
        sha1 = "83f0a0ec378bf3246178b6c2ad9136f135b1c962";
      };
    }

    {
      name = "ieee754-1.1.13.tgz";
      path = fetchurl {
        name = "ieee754-1.1.13.tgz";
        url  = "https://registry.yarnpkg.com/ieee754/-/ieee754-1.1.13.tgz";
        sha1 = "ec168558e95aa181fd87d37f55c32bbcb6708b84";
      };
    }

    {
      name = "iferr-0.1.5.tgz";
      path = fetchurl {
        name = "iferr-0.1.5.tgz";
        url  = "https://registry.yarnpkg.com/iferr/-/iferr-0.1.5.tgz";
        sha1 = "c60eed69e6d8fdb6b3104a1fcbca1c192dc5b501";
      };
    }

    {
      name = "import-local-2.0.0.tgz";
      path = fetchurl {
        name = "import-local-2.0.0.tgz";
        url  = "https://registry.yarnpkg.com/import-local/-/import-local-2.0.0.tgz";
        sha1 = "55070be38a5993cf18ef6db7e961f5bee5c5a09d";
      };
    }

    {
      name = "imurmurhash-0.1.4.tgz";
      path = fetchurl {
        name = "imurmurhash-0.1.4.tgz";
        url  = "https://registry.yarnpkg.com/imurmurhash/-/imurmurhash-0.1.4.tgz";
        sha1 = "9218b9b2b928a238b13dc4fb6b6d576f231453ea";
      };
    }

    {
      name = "in-publish-2.0.0.tgz";
      path = fetchurl {
        name = "in-publish-2.0.0.tgz";
        url  = "https://registry.yarnpkg.com/in-publish/-/in-publish-2.0.0.tgz";
        sha1 = "e20ff5e3a2afc2690320b6dc552682a9c7fadf51";
      };
    }

    {
      name = "indent-string-2.1.0.tgz";
      path = fetchurl {
        name = "indent-string-2.1.0.tgz";
        url  = "https://registry.yarnpkg.com/indent-string/-/indent-string-2.1.0.tgz";
        sha1 = "8e2d48348742121b4a8218b7a137e9a52049dc80";
      };
    }

    {
      name = "infer-owner-1.0.4.tgz";
      path = fetchurl {
        name = "infer-owner-1.0.4.tgz";
        url  = "https://registry.yarnpkg.com/infer-owner/-/infer-owner-1.0.4.tgz";
        sha1 = "c4cefcaa8e51051c2a40ba2ce8a3d27295af9467";
      };
    }

    {
      name = "inflight-1.0.6.tgz";
      path = fetchurl {
        name = "inflight-1.0.6.tgz";
        url  = "https://registry.yarnpkg.com/inflight/-/inflight-1.0.6.tgz";
        sha1 = "49bd6331d7d02d0c09bc910a1075ba8165b56df9";
      };
    }

    {
      name = "inherits-2.0.4.tgz";
      path = fetchurl {
        name = "inherits-2.0.4.tgz";
        url  = "https://registry.yarnpkg.com/inherits/-/inherits-2.0.4.tgz";
        sha1 = "0fa2c64f932917c3433a0ded55363aae37416b7c";
      };
    }

    {
      name = "inherits-2.0.1.tgz";
      path = fetchurl {
        name = "inherits-2.0.1.tgz";
        url  = "https://registry.yarnpkg.com/inherits/-/inherits-2.0.1.tgz";
        sha1 = "b17d08d326b4423e568eff719f91b0b1cbdf69f1";
      };
    }

    {
      name = "inherits-2.0.3.tgz";
      path = fetchurl {
        name = "inherits-2.0.3.tgz";
        url  = "https://registry.yarnpkg.com/inherits/-/inherits-2.0.3.tgz";
        sha1 = "633c2c83e3da42a502f52466022480f4208261de";
      };
    }

    {
      name = "ini-1.3.5.tgz";
      path = fetchurl {
        name = "ini-1.3.5.tgz";
        url  = "https://registry.yarnpkg.com/ini/-/ini-1.3.5.tgz";
        sha1 = "eee25f56db1c9ec6085e0c22778083f596abf927";
      };
    }

    {
      name = "internal-ip-4.3.0.tgz";
      path = fetchurl {
        name = "internal-ip-4.3.0.tgz";
        url  = "https://registry.yarnpkg.com/internal-ip/-/internal-ip-4.3.0.tgz";
        sha1 = "845452baad9d2ca3b69c635a137acb9a0dad0907";
      };
    }

    {
      name = "interpret-1.2.0.tgz";
      path = fetchurl {
        name = "interpret-1.2.0.tgz";
        url  = "https://registry.yarnpkg.com/interpret/-/interpret-1.2.0.tgz";
        sha1 = "d5061a6224be58e8083985f5014d844359576296";
      };
    }

    {
      name = "invert-kv-1.0.0.tgz";
      path = fetchurl {
        name = "invert-kv-1.0.0.tgz";
        url  = "https://registry.yarnpkg.com/invert-kv/-/invert-kv-1.0.0.tgz";
        sha1 = "104a8e4aaca6d3d8cd157a8ef8bfab2d7a3ffdb6";
      };
    }

    {
      name = "invert-kv-2.0.0.tgz";
      path = fetchurl {
        name = "invert-kv-2.0.0.tgz";
        url  = "https://registry.yarnpkg.com/invert-kv/-/invert-kv-2.0.0.tgz";
        sha1 = "7393f5afa59ec9ff5f67a27620d11c226e3eec02";
      };
    }

    {
      name = "ip-regex-2.1.0.tgz";
      path = fetchurl {
        name = "ip-regex-2.1.0.tgz";
        url  = "https://registry.yarnpkg.com/ip-regex/-/ip-regex-2.1.0.tgz";
        sha1 = "fa78bf5d2e6913c911ce9f819ee5146bb6d844e9";
      };
    }

    {
      name = "ip-1.1.5.tgz";
      path = fetchurl {
        name = "ip-1.1.5.tgz";
        url  = "https://registry.yarnpkg.com/ip/-/ip-1.1.5.tgz";
        sha1 = "bdded70114290828c0a039e72ef25f5aaec4354a";
      };
    }

    {
      name = "ipaddr.js-1.9.0.tgz";
      path = fetchurl {
        name = "ipaddr.js-1.9.0.tgz";
        url  = "https://registry.yarnpkg.com/ipaddr.js/-/ipaddr.js-1.9.0.tgz";
        sha1 = "37df74e430a0e47550fe54a2defe30d8acd95f65";
      };
    }

    {
      name = "ipaddr.js-1.9.1.tgz";
      path = fetchurl {
        name = "ipaddr.js-1.9.1.tgz";
        url  = "https://registry.yarnpkg.com/ipaddr.js/-/ipaddr.js-1.9.1.tgz";
        sha1 = "bff38543eeb8984825079ff3a2a8e6cbd46781b3";
      };
    }

    {
      name = "is-absolute-url-3.0.3.tgz";
      path = fetchurl {
        name = "is-absolute-url-3.0.3.tgz";
        url  = "https://registry.yarnpkg.com/is-absolute-url/-/is-absolute-url-3.0.3.tgz";
        sha1 = "96c6a22b6a23929b11ea0afb1836c36ad4a5d698";
      };
    }

    {
      name = "is-accessor-descriptor-0.1.6.tgz";
      path = fetchurl {
        name = "is-accessor-descriptor-0.1.6.tgz";
        url  = "https://registry.yarnpkg.com/is-accessor-descriptor/-/is-accessor-descriptor-0.1.6.tgz";
        sha1 = "a9e12cb3ae8d876727eeef3843f8a0897b5c98d6";
      };
    }

    {
      name = "is-accessor-descriptor-1.0.0.tgz";
      path = fetchurl {
        name = "is-accessor-descriptor-1.0.0.tgz";
        url  = "https://registry.yarnpkg.com/is-accessor-descriptor/-/is-accessor-descriptor-1.0.0.tgz";
        sha1 = "169c2f6d3df1f992618072365c9b0ea1f6878656";
      };
    }

    {
      name = "is-arguments-1.0.4.tgz";
      path = fetchurl {
        name = "is-arguments-1.0.4.tgz";
        url  = "https://registry.yarnpkg.com/is-arguments/-/is-arguments-1.0.4.tgz";
        sha1 = "3faf966c7cba0ff437fb31f6250082fcf0448cf3";
      };
    }

    {
      name = "is-arrayish-0.2.1.tgz";
      path = fetchurl {
        name = "is-arrayish-0.2.1.tgz";
        url  = "https://registry.yarnpkg.com/is-arrayish/-/is-arrayish-0.2.1.tgz";
        sha1 = "77c99840527aa8ecb1a8ba697b80645a7a926a9d";
      };
    }

    {
      name = "is-binary-path-1.0.1.tgz";
      path = fetchurl {
        name = "is-binary-path-1.0.1.tgz";
        url  = "https://registry.yarnpkg.com/is-binary-path/-/is-binary-path-1.0.1.tgz";
        sha1 = "75f16642b480f187a711c814161fd3a4a7655898";
      };
    }

    {
      name = "is-buffer-1.1.6.tgz";
      path = fetchurl {
        name = "is-buffer-1.1.6.tgz";
        url  = "https://registry.yarnpkg.com/is-buffer/-/is-buffer-1.1.6.tgz";
        sha1 = "efaa2ea9daa0d7ab2ea13a97b2b8ad51fefbe8be";
      };
    }

    {
      name = "is-callable-1.1.5.tgz";
      path = fetchurl {
        name = "is-callable-1.1.5.tgz";
        url  = "https://registry.yarnpkg.com/is-callable/-/is-callable-1.1.5.tgz";
        sha1 = "f7e46b596890456db74e7f6e976cb3273d06faab";
      };
    }

    {
      name = "is-data-descriptor-0.1.4.tgz";
      path = fetchurl {
        name = "is-data-descriptor-0.1.4.tgz";
        url  = "https://registry.yarnpkg.com/is-data-descriptor/-/is-data-descriptor-0.1.4.tgz";
        sha1 = "0b5ee648388e2c860282e793f1856fec3f301b56";
      };
    }

    {
      name = "is-data-descriptor-1.0.0.tgz";
      path = fetchurl {
        name = "is-data-descriptor-1.0.0.tgz";
        url  = "https://registry.yarnpkg.com/is-data-descriptor/-/is-data-descriptor-1.0.0.tgz";
        sha1 = "d84876321d0e7add03990406abbbbd36ba9268c7";
      };
    }

    {
      name = "is-date-object-1.0.2.tgz";
      path = fetchurl {
        name = "is-date-object-1.0.2.tgz";
        url  = "https://registry.yarnpkg.com/is-date-object/-/is-date-object-1.0.2.tgz";
        sha1 = "bda736f2cd8fd06d32844e7743bfa7494c3bfd7e";
      };
    }

    {
      name = "is-descriptor-0.1.6.tgz";
      path = fetchurl {
        name = "is-descriptor-0.1.6.tgz";
        url  = "https://registry.yarnpkg.com/is-descriptor/-/is-descriptor-0.1.6.tgz";
        sha1 = "366d8240dde487ca51823b1ab9f07a10a78251ca";
      };
    }

    {
      name = "is-descriptor-1.0.2.tgz";
      path = fetchurl {
        name = "is-descriptor-1.0.2.tgz";
        url  = "https://registry.yarnpkg.com/is-descriptor/-/is-descriptor-1.0.2.tgz";
        sha1 = "3b159746a66604b04f8c81524ba365c5f14d86ec";
      };
    }

    {
      name = "is-extendable-0.1.1.tgz";
      path = fetchurl {
        name = "is-extendable-0.1.1.tgz";
        url  = "https://registry.yarnpkg.com/is-extendable/-/is-extendable-0.1.1.tgz";
        sha1 = "62b110e289a471418e3ec36a617d472e301dfc89";
      };
    }

    {
      name = "is-extendable-1.0.1.tgz";
      path = fetchurl {
        name = "is-extendable-1.0.1.tgz";
        url  = "https://registry.yarnpkg.com/is-extendable/-/is-extendable-1.0.1.tgz";
        sha1 = "a7470f9e426733d81bd81e1155264e3a3507cab4";
      };
    }

    {
      name = "is-extglob-2.1.1.tgz";
      path = fetchurl {
        name = "is-extglob-2.1.1.tgz";
        url  = "https://registry.yarnpkg.com/is-extglob/-/is-extglob-2.1.1.tgz";
        sha1 = "a88c02535791f02ed37c76a1b9ea9773c833f8c2";
      };
    }

    {
      name = "is-finite-1.1.0.tgz";
      path = fetchurl {
        name = "is-finite-1.1.0.tgz";
        url  = "https://registry.yarnpkg.com/is-finite/-/is-finite-1.1.0.tgz";
        sha1 = "904135c77fb42c0641d6aa1bcdbc4daa8da082f3";
      };
    }

    {
      name = "is-fullwidth-code-point-1.0.0.tgz";
      path = fetchurl {
        name = "is-fullwidth-code-point-1.0.0.tgz";
        url  = "https://registry.yarnpkg.com/is-fullwidth-code-point/-/is-fullwidth-code-point-1.0.0.tgz";
        sha1 = "ef9e31386f031a7f0d643af82fde50c457ef00cb";
      };
    }

    {
      name = "is-fullwidth-code-point-2.0.0.tgz";
      path = fetchurl {
        name = "is-fullwidth-code-point-2.0.0.tgz";
        url  = "https://registry.yarnpkg.com/is-fullwidth-code-point/-/is-fullwidth-code-point-2.0.0.tgz";
        sha1 = "a3b30a5c4f199183167aaab93beefae3ddfb654f";
      };
    }

    {
      name = "is-glob-3.1.0.tgz";
      path = fetchurl {
        name = "is-glob-3.1.0.tgz";
        url  = "https://registry.yarnpkg.com/is-glob/-/is-glob-3.1.0.tgz";
        sha1 = "7ba5ae24217804ac70707b96922567486cc3e84a";
      };
    }

    {
      name = "is-glob-4.0.1.tgz";
      path = fetchurl {
        name = "is-glob-4.0.1.tgz";
        url  = "https://registry.yarnpkg.com/is-glob/-/is-glob-4.0.1.tgz";
        sha1 = "7567dbe9f2f5e2467bc77ab83c4a29482407a5dc";
      };
    }

    {
      name = "is-number-3.0.0.tgz";
      path = fetchurl {
        name = "is-number-3.0.0.tgz";
        url  = "https://registry.yarnpkg.com/is-number/-/is-number-3.0.0.tgz";
        sha1 = "24fd6201a4782cf50561c810276afc7d12d71195";
      };
    }

    {
      name = "is-path-cwd-2.2.0.tgz";
      path = fetchurl {
        name = "is-path-cwd-2.2.0.tgz";
        url  = "https://registry.yarnpkg.com/is-path-cwd/-/is-path-cwd-2.2.0.tgz";
        sha1 = "67d43b82664a7b5191fd9119127eb300048a9fdb";
      };
    }

    {
      name = "is-path-in-cwd-2.1.0.tgz";
      path = fetchurl {
        name = "is-path-in-cwd-2.1.0.tgz";
        url  = "https://registry.yarnpkg.com/is-path-in-cwd/-/is-path-in-cwd-2.1.0.tgz";
        sha1 = "bfe2dca26c69f397265a4009963602935a053acb";
      };
    }

    {
      name = "is-path-inside-2.1.0.tgz";
      path = fetchurl {
        name = "is-path-inside-2.1.0.tgz";
        url  = "https://registry.yarnpkg.com/is-path-inside/-/is-path-inside-2.1.0.tgz";
        sha1 = "7c9810587d659a40d27bcdb4d5616eab059494b2";
      };
    }

    {
      name = "is-plain-object-2.0.4.tgz";
      path = fetchurl {
        name = "is-plain-object-2.0.4.tgz";
        url  = "https://registry.yarnpkg.com/is-plain-object/-/is-plain-object-2.0.4.tgz";
        sha1 = "2c163b3fafb1b606d9d17928f05c2a1c38e07677";
      };
    }

    {
      name = "is-regex-1.0.5.tgz";
      path = fetchurl {
        name = "is-regex-1.0.5.tgz";
        url  = "https://registry.yarnpkg.com/is-regex/-/is-regex-1.0.5.tgz";
        sha1 = "39d589a358bf18967f726967120b8fc1aed74eae";
      };
    }

    {
      name = "is-stream-1.1.0.tgz";
      path = fetchurl {
        name = "is-stream-1.1.0.tgz";
        url  = "https://registry.yarnpkg.com/is-stream/-/is-stream-1.1.0.tgz";
        sha1 = "12d4a3dd4e68e0b79ceb8dbc84173ae80d91ca44";
      };
    }

    {
      name = "is-symbol-1.0.3.tgz";
      path = fetchurl {
        name = "is-symbol-1.0.3.tgz";
        url  = "https://registry.yarnpkg.com/is-symbol/-/is-symbol-1.0.3.tgz";
        sha1 = "38e1014b9e6329be0de9d24a414fd7441ec61937";
      };
    }

    {
      name = "is-typedarray-1.0.0.tgz";
      path = fetchurl {
        name = "is-typedarray-1.0.0.tgz";
        url  = "https://registry.yarnpkg.com/is-typedarray/-/is-typedarray-1.0.0.tgz";
        sha1 = "e479c80858df0c1b11ddda6940f96011fcda4a9a";
      };
    }

    {
      name = "is-utf8-0.2.1.tgz";
      path = fetchurl {
        name = "is-utf8-0.2.1.tgz";
        url  = "https://registry.yarnpkg.com/is-utf8/-/is-utf8-0.2.1.tgz";
        sha1 = "4b0da1442104d1b336340e80797e865cf39f7d72";
      };
    }

    {
      name = "is-windows-1.0.2.tgz";
      path = fetchurl {
        name = "is-windows-1.0.2.tgz";
        url  = "https://registry.yarnpkg.com/is-windows/-/is-windows-1.0.2.tgz";
        sha1 = "d1850eb9791ecd18e6182ce12a30f396634bb19d";
      };
    }

    {
      name = "is-wsl-1.1.0.tgz";
      path = fetchurl {
        name = "is-wsl-1.1.0.tgz";
        url  = "https://registry.yarnpkg.com/is-wsl/-/is-wsl-1.1.0.tgz";
        sha1 = "1f16e4aa22b04d1336b66188a66af3c600c3a66d";
      };
    }

    {
      name = "isarray-1.0.0.tgz";
      path = fetchurl {
        name = "isarray-1.0.0.tgz";
        url  = "https://registry.yarnpkg.com/isarray/-/isarray-1.0.0.tgz";
        sha1 = "bb935d48582cba168c06834957a54a3e07124f11";
      };
    }

    {
      name = "isexe-2.0.0.tgz";
      path = fetchurl {
        name = "isexe-2.0.0.tgz";
        url  = "https://registry.yarnpkg.com/isexe/-/isexe-2.0.0.tgz";
        sha1 = "e8fbf374dc556ff8947a10dcb0572d633f2cfa10";
      };
    }

    {
      name = "isobject-2.1.0.tgz";
      path = fetchurl {
        name = "isobject-2.1.0.tgz";
        url  = "https://registry.yarnpkg.com/isobject/-/isobject-2.1.0.tgz";
        sha1 = "f065561096a3f1da2ef46272f815c840d87e0c89";
      };
    }

    {
      name = "isobject-3.0.1.tgz";
      path = fetchurl {
        name = "isobject-3.0.1.tgz";
        url  = "https://registry.yarnpkg.com/isobject/-/isobject-3.0.1.tgz";
        sha1 = "4e431e92b11a9731636aa1f9c8d1ccbcfdab78df";
      };
    }

    {
      name = "isstream-0.1.2.tgz";
      path = fetchurl {
        name = "isstream-0.1.2.tgz";
        url  = "https://registry.yarnpkg.com/isstream/-/isstream-0.1.2.tgz";
        sha1 = "47e63f7af55afa6f92e1500e690eb8b8529c099a";
      };
    }

    {
      name = "iterall-1.3.0.tgz";
      path = fetchurl {
        name = "iterall-1.3.0.tgz";
        url  = "https://registry.yarnpkg.com/iterall/-/iterall-1.3.0.tgz";
        sha1 = "afcb08492e2915cbd8a0884eb93a8c94d0d72fea";
      };
    }

    {
      name = "jquery-3.4.1.tgz";
      path = fetchurl {
        name = "jquery-3.4.1.tgz";
        url  = "https://registry.yarnpkg.com/jquery/-/jquery-3.4.1.tgz";
        sha1 = "714f1f8d9dde4bdfa55764ba37ef214630d80ef2";
      };
    }

    {
      name = "js-base64-2.5.2.tgz";
      path = fetchurl {
        name = "js-base64-2.5.2.tgz";
        url  = "https://registry.yarnpkg.com/js-base64/-/js-base64-2.5.2.tgz";
        sha1 = "313b6274dda718f714d00b3330bbae6e38e90209";
      };
    }

    {
      name = "js-string-escape-1.0.1.tgz";
      path = fetchurl {
        name = "js-string-escape-1.0.1.tgz";
        url  = "https://registry.yarnpkg.com/js-string-escape/-/js-string-escape-1.0.1.tgz";
        sha1 = "e2625badbc0d67c7533e9edc1068c587ae4137ef";
      };
    }

    {
      name = "js-tokens-3.0.2.tgz";
      path = fetchurl {
        name = "js-tokens-3.0.2.tgz";
        url  = "https://registry.yarnpkg.com/js-tokens/-/js-tokens-3.0.2.tgz";
        sha1 = "9866df395102130e38f7f996bceb65443209c25b";
      };
    }

    {
      name = "jsbn-0.1.1.tgz";
      path = fetchurl {
        name = "jsbn-0.1.1.tgz";
        url  = "https://registry.yarnpkg.com/jsbn/-/jsbn-0.1.1.tgz";
        sha1 = "a5e654c2e5a2deb5f201d96cefbca80c0ef2f513";
      };
    }

    {
      name = "jsesc-0.5.0.tgz";
      path = fetchurl {
        name = "jsesc-0.5.0.tgz";
        url  = "https://registry.yarnpkg.com/jsesc/-/jsesc-0.5.0.tgz";
        sha1 = "e7dee66e35d6fc16f710fe91d5cf69f70f08911d";
      };
    }

    {
      name = "json-parse-better-errors-1.0.2.tgz";
      path = fetchurl {
        name = "json-parse-better-errors-1.0.2.tgz";
        url  = "https://registry.yarnpkg.com/json-parse-better-errors/-/json-parse-better-errors-1.0.2.tgz";
        sha1 = "bb867cfb3450e69107c131d1c514bab3dc8bcaa9";
      };
    }

    {
      name = "json-schema-traverse-0.3.1.tgz";
      path = fetchurl {
        name = "json-schema-traverse-0.3.1.tgz";
        url  = "https://registry.yarnpkg.com/json-schema-traverse/-/json-schema-traverse-0.3.1.tgz";
        sha1 = "349a6d44c53a51de89b40805c5d5e59b417d3340";
      };
    }

    {
      name = "json-schema-traverse-0.4.1.tgz";
      path = fetchurl {
        name = "json-schema-traverse-0.4.1.tgz";
        url  = "https://registry.yarnpkg.com/json-schema-traverse/-/json-schema-traverse-0.4.1.tgz";
        sha1 = "69f6a87d9513ab8bb8fe63bdb0979c448e684660";
      };
    }

    {
      name = "json-schema-0.2.3.tgz";
      path = fetchurl {
        name = "json-schema-0.2.3.tgz";
        url  = "https://registry.yarnpkg.com/json-schema/-/json-schema-0.2.3.tgz";
        sha1 = "b480c892e59a2f05954ce727bd3f2a4e882f9e13";
      };
    }

    {
      name = "json-stringify-safe-5.0.1.tgz";
      path = fetchurl {
        name = "json-stringify-safe-5.0.1.tgz";
        url  = "https://registry.yarnpkg.com/json-stringify-safe/-/json-stringify-safe-5.0.1.tgz";
        sha1 = "1296a2d58fd45f19a0f6ce01d65701e2c735b6eb";
      };
    }

    {
      name = "json3-3.3.3.tgz";
      path = fetchurl {
        name = "json3-3.3.3.tgz";
        url  = "https://registry.yarnpkg.com/json3/-/json3-3.3.3.tgz";
        sha1 = "7fc10e375fc5ae42c4705a5cc0aa6f62be305b81";
      };
    }

    {
      name = "json5-0.5.1.tgz";
      path = fetchurl {
        name = "json5-0.5.1.tgz";
        url  = "https://registry.yarnpkg.com/json5/-/json5-0.5.1.tgz";
        sha1 = "1eade7acc012034ad84e2396767ead9fa5495821";
      };
    }

    {
      name = "json5-1.0.1.tgz";
      path = fetchurl {
        name = "json5-1.0.1.tgz";
        url  = "https://registry.yarnpkg.com/json5/-/json5-1.0.1.tgz";
        sha1 = "779fb0018604fa854eacbf6252180d83543e3dbe";
      };
    }

    {
      name = "jsprim-1.4.1.tgz";
      path = fetchurl {
        name = "jsprim-1.4.1.tgz";
        url  = "https://registry.yarnpkg.com/jsprim/-/jsprim-1.4.1.tgz";
        sha1 = "313e66bc1e5cc06e438bc1b7499c2e5c56acb6a2";
      };
    }

    {
      name = "killable-1.0.1.tgz";
      path = fetchurl {
        name = "killable-1.0.1.tgz";
        url  = "https://registry.yarnpkg.com/killable/-/killable-1.0.1.tgz";
        sha1 = "4c8ce441187a061c7474fb87ca08e2a638194892";
      };
    }

    {
      name = "kind-of-3.2.2.tgz";
      path = fetchurl {
        name = "kind-of-3.2.2.tgz";
        url  = "https://registry.yarnpkg.com/kind-of/-/kind-of-3.2.2.tgz";
        sha1 = "31ea21a734bab9bbb0f32466d893aea51e4a3c64";
      };
    }

    {
      name = "kind-of-4.0.0.tgz";
      path = fetchurl {
        name = "kind-of-4.0.0.tgz";
        url  = "https://registry.yarnpkg.com/kind-of/-/kind-of-4.0.0.tgz";
        sha1 = "20813df3d712928b207378691a45066fae72dd57";
      };
    }

    {
      name = "kind-of-5.1.0.tgz";
      path = fetchurl {
        name = "kind-of-5.1.0.tgz";
        url  = "https://registry.yarnpkg.com/kind-of/-/kind-of-5.1.0.tgz";
        sha1 = "729c91e2d857b7a419a1f9aa65685c4c33f5845d";
      };
    }

    {
      name = "kind-of-6.0.3.tgz";
      path = fetchurl {
        name = "kind-of-6.0.3.tgz";
        url  = "https://registry.yarnpkg.com/kind-of/-/kind-of-6.0.3.tgz";
        sha1 = "07c05034a6c349fa06e24fa35aa76db4580ce4dd";
      };
    }

    {
      name = "lcid-1.0.0.tgz";
      path = fetchurl {
        name = "lcid-1.0.0.tgz";
        url  = "https://registry.yarnpkg.com/lcid/-/lcid-1.0.0.tgz";
        sha1 = "308accafa0bc483a3867b4b6f2b9506251d1b835";
      };
    }

    {
      name = "lcid-2.0.0.tgz";
      path = fetchurl {
        name = "lcid-2.0.0.tgz";
        url  = "https://registry.yarnpkg.com/lcid/-/lcid-2.0.0.tgz";
        sha1 = "6ef5d2df60e52f82eb228a4c373e8d1f397253cf";
      };
    }

    {
      name = "load-json-file-1.1.0.tgz";
      path = fetchurl {
        name = "load-json-file-1.1.0.tgz";
        url  = "https://registry.yarnpkg.com/load-json-file/-/load-json-file-1.1.0.tgz";
        sha1 = "956905708d58b4bab4c2261b04f59f31c99374c0";
      };
    }

    {
      name = "loader-runner-2.4.0.tgz";
      path = fetchurl {
        name = "loader-runner-2.4.0.tgz";
        url  = "https://registry.yarnpkg.com/loader-runner/-/loader-runner-2.4.0.tgz";
        sha1 = "ed47066bfe534d7e84c4c7b9998c2a75607d9357";
      };
    }

    {
      name = "loader-utils-1.2.3.tgz";
      path = fetchurl {
        name = "loader-utils-1.2.3.tgz";
        url  = "https://registry.yarnpkg.com/loader-utils/-/loader-utils-1.2.3.tgz";
        sha1 = "1ff5dc6911c9f0a062531a4c04b609406108c2c7";
      };
    }

    {
      name = "loader-utils-0.2.17.tgz";
      path = fetchurl {
        name = "loader-utils-0.2.17.tgz";
        url  = "https://registry.yarnpkg.com/loader-utils/-/loader-utils-0.2.17.tgz";
        sha1 = "f86e6374d43205a6e6c60e9196f17c0299bfb348";
      };
    }

    {
      name = "locate-path-3.0.0.tgz";
      path = fetchurl {
        name = "locate-path-3.0.0.tgz";
        url  = "https://registry.yarnpkg.com/locate-path/-/locate-path-3.0.0.tgz";
        sha1 = "dbec3b3ab759758071b58fe59fc41871af21400e";
      };
    }

    {
      name = "lodash.difference-4.5.0.tgz";
      path = fetchurl {
        name = "lodash.difference-4.5.0.tgz";
        url  = "https://registry.yarnpkg.com/lodash.difference/-/lodash.difference-4.5.0.tgz";
        sha1 = "9ccb4e505d486b91651345772885a2df27fd017c";
      };
    }

    {
      name = "lodash-4.17.15.tgz";
      path = fetchurl {
        name = "lodash-4.17.15.tgz";
        url  = "https://registry.yarnpkg.com/lodash/-/lodash-4.17.15.tgz";
        sha1 = "b447f6670a0455bbfeedd11392eff330ea097548";
      };
    }

    {
      name = "loglevel-1.6.7.tgz";
      path = fetchurl {
        name = "loglevel-1.6.7.tgz";
        url  = "https://registry.yarnpkg.com/loglevel/-/loglevel-1.6.7.tgz";
        sha1 = "b3e034233188c68b889f5b862415306f565e2c56";
      };
    }

    {
      name = "loud-rejection-1.6.0.tgz";
      path = fetchurl {
        name = "loud-rejection-1.6.0.tgz";
        url  = "https://registry.yarnpkg.com/loud-rejection/-/loud-rejection-1.6.0.tgz";
        sha1 = "5b46f80147edee578870f086d04821cf998e551f";
      };
    }

    {
      name = "lower-case-1.1.4.tgz";
      path = fetchurl {
        name = "lower-case-1.1.4.tgz";
        url  = "https://registry.yarnpkg.com/lower-case/-/lower-case-1.1.4.tgz";
        sha1 = "9a2cabd1b9e8e0ae993a4bf7d5875c39c42e8eac";
      };
    }

    {
      name = "lru-cache-4.1.5.tgz";
      path = fetchurl {
        name = "lru-cache-4.1.5.tgz";
        url  = "https://registry.yarnpkg.com/lru-cache/-/lru-cache-4.1.5.tgz";
        sha1 = "8bbe50ea85bed59bc9e33dcab8235ee9bcf443cd";
      };
    }

    {
      name = "lru-cache-5.1.1.tgz";
      path = fetchurl {
        name = "lru-cache-5.1.1.tgz";
        url  = "https://registry.yarnpkg.com/lru-cache/-/lru-cache-5.1.1.tgz";
        sha1 = "1da27e6710271947695daf6848e847f01d84b920";
      };
    }

    {
      name = "make-dir-2.1.0.tgz";
      path = fetchurl {
        name = "make-dir-2.1.0.tgz";
        url  = "https://registry.yarnpkg.com/make-dir/-/make-dir-2.1.0.tgz";
        sha1 = "5f0310e18b8be898cc07009295a30ae41e91e6f5";
      };
    }

    {
      name = "mamacro-0.0.3.tgz";
      path = fetchurl {
        name = "mamacro-0.0.3.tgz";
        url  = "https://registry.yarnpkg.com/mamacro/-/mamacro-0.0.3.tgz";
        sha1 = "ad2c9576197c9f1abf308d0787865bd975a3f3e4";
      };
    }

    {
      name = "map-age-cleaner-0.1.3.tgz";
      path = fetchurl {
        name = "map-age-cleaner-0.1.3.tgz";
        url  = "https://registry.yarnpkg.com/map-age-cleaner/-/map-age-cleaner-0.1.3.tgz";
        sha1 = "7d583a7306434c055fe474b0f45078e6e1b4b92a";
      };
    }

    {
      name = "map-cache-0.2.2.tgz";
      path = fetchurl {
        name = "map-cache-0.2.2.tgz";
        url  = "https://registry.yarnpkg.com/map-cache/-/map-cache-0.2.2.tgz";
        sha1 = "c32abd0bd6525d9b051645bb4f26ac5dc98a0dbf";
      };
    }

    {
      name = "map-obj-1.0.1.tgz";
      path = fetchurl {
        name = "map-obj-1.0.1.tgz";
        url  = "https://registry.yarnpkg.com/map-obj/-/map-obj-1.0.1.tgz";
        sha1 = "d933ceb9205d82bdcf4886f6742bdc2b4dea146d";
      };
    }

    {
      name = "map-visit-1.0.0.tgz";
      path = fetchurl {
        name = "map-visit-1.0.0.tgz";
        url  = "https://registry.yarnpkg.com/map-visit/-/map-visit-1.0.0.tgz";
        sha1 = "ecdca8f13144e660f1b5bd41f12f3479d98dfb8f";
      };
    }

    {
      name = "md5.js-1.3.5.tgz";
      path = fetchurl {
        name = "md5.js-1.3.5.tgz";
        url  = "https://registry.yarnpkg.com/md5.js/-/md5.js-1.3.5.tgz";
        sha1 = "b5d07b8e3216e3e27cd728d72f70d1e6a342005f";
      };
    }

    {
      name = "media-typer-0.3.0.tgz";
      path = fetchurl {
        name = "media-typer-0.3.0.tgz";
        url  = "https://registry.yarnpkg.com/media-typer/-/media-typer-0.3.0.tgz";
        sha1 = "8710d7af0aa626f8fffa1ce00168545263255748";
      };
    }

    {
      name = "mem-4.3.0.tgz";
      path = fetchurl {
        name = "mem-4.3.0.tgz";
        url  = "https://registry.yarnpkg.com/mem/-/mem-4.3.0.tgz";
        sha1 = "461af497bc4ae09608cdb2e60eefb69bff744178";
      };
    }

    {
      name = "memory-fs-0.4.1.tgz";
      path = fetchurl {
        name = "memory-fs-0.4.1.tgz";
        url  = "https://registry.yarnpkg.com/memory-fs/-/memory-fs-0.4.1.tgz";
        sha1 = "3a9a20b8462523e447cfbc7e8bb80ed667bfc552";
      };
    }

    {
      name = "memory-fs-0.5.0.tgz";
      path = fetchurl {
        name = "memory-fs-0.5.0.tgz";
        url  = "https://registry.yarnpkg.com/memory-fs/-/memory-fs-0.5.0.tgz";
        sha1 = "324c01288b88652966d161db77838720845a8e3c";
      };
    }

    {
      name = "meow-3.7.0.tgz";
      path = fetchurl {
        name = "meow-3.7.0.tgz";
        url  = "https://registry.yarnpkg.com/meow/-/meow-3.7.0.tgz";
        sha1 = "72cb668b425228290abbfa856892587308a801fb";
      };
    }

    {
      name = "merge-descriptors-1.0.1.tgz";
      path = fetchurl {
        name = "merge-descriptors-1.0.1.tgz";
        url  = "https://registry.yarnpkg.com/merge-descriptors/-/merge-descriptors-1.0.1.tgz";
        sha1 = "b00aaa556dd8b44568150ec9d1b953f3f90cbb61";
      };
    }

    {
      name = "methods-1.1.2.tgz";
      path = fetchurl {
        name = "methods-1.1.2.tgz";
        url  = "https://registry.yarnpkg.com/methods/-/methods-1.1.2.tgz";
        sha1 = "5529a4d67654134edcc5266656835b0f851afcee";
      };
    }

    {
      name = "micromatch-3.1.10.tgz";
      path = fetchurl {
        name = "micromatch-3.1.10.tgz";
        url  = "https://registry.yarnpkg.com/micromatch/-/micromatch-3.1.10.tgz";
        sha1 = "70859bc95c9840952f359a068a3fc49f9ecfac23";
      };
    }

    {
      name = "miller-rabin-4.0.1.tgz";
      path = fetchurl {
        name = "miller-rabin-4.0.1.tgz";
        url  = "https://registry.yarnpkg.com/miller-rabin/-/miller-rabin-4.0.1.tgz";
        sha1 = "f080351c865b0dc562a8462966daa53543c78a4d";
      };
    }

    {
      name = "mime-db-1.43.0.tgz";
      path = fetchurl {
        name = "mime-db-1.43.0.tgz";
        url  = "https://registry.yarnpkg.com/mime-db/-/mime-db-1.43.0.tgz";
        sha1 = "0a12e0502650e473d735535050e7c8f4eb4fae58";
      };
    }

    {
      name = "mime-types-2.1.26.tgz";
      path = fetchurl {
        name = "mime-types-2.1.26.tgz";
        url  = "https://registry.yarnpkg.com/mime-types/-/mime-types-2.1.26.tgz";
        sha1 = "9c921fc09b7e149a65dfdc0da4d20997200b0a06";
      };
    }

    {
      name = "mime-1.6.0.tgz";
      path = fetchurl {
        name = "mime-1.6.0.tgz";
        url  = "https://registry.yarnpkg.com/mime/-/mime-1.6.0.tgz";
        sha1 = "32cd9e5c64553bd58d19a568af452acff04981b1";
      };
    }

    {
      name = "mime-2.4.4.tgz";
      path = fetchurl {
        name = "mime-2.4.4.tgz";
        url  = "https://registry.yarnpkg.com/mime/-/mime-2.4.4.tgz";
        sha1 = "bd7b91135fc6b01cde3e9bae33d659b63d8857e5";
      };
    }

    {
      name = "mimic-fn-2.1.0.tgz";
      path = fetchurl {
        name = "mimic-fn-2.1.0.tgz";
        url  = "https://registry.yarnpkg.com/mimic-fn/-/mimic-fn-2.1.0.tgz";
        sha1 = "7ed2c2ccccaf84d3ffcb7a69b57711fc2083401b";
      };
    }

    {
      name = "minimalistic-assert-1.0.1.tgz";
      path = fetchurl {
        name = "minimalistic-assert-1.0.1.tgz";
        url  = "https://registry.yarnpkg.com/minimalistic-assert/-/minimalistic-assert-1.0.1.tgz";
        sha1 = "2e194de044626d4a10e7f7fbc00ce73e83e4d5c7";
      };
    }

    {
      name = "minimalistic-crypto-utils-1.0.1.tgz";
      path = fetchurl {
        name = "minimalistic-crypto-utils-1.0.1.tgz";
        url  = "https://registry.yarnpkg.com/minimalistic-crypto-utils/-/minimalistic-crypto-utils-1.0.1.tgz";
        sha1 = "f6c00c1c0b082246e5c4d99dfb8c7c083b2b582a";
      };
    }

    {
      name = "minimatch-3.0.4.tgz";
      path = fetchurl {
        name = "minimatch-3.0.4.tgz";
        url  = "https://registry.yarnpkg.com/minimatch/-/minimatch-3.0.4.tgz";
        sha1 = "5166e286457f03306064be5497e8dbb0c3d32083";
      };
    }

    {
      name = "minimist-0.0.8.tgz";
      path = fetchurl {
        name = "minimist-0.0.8.tgz";
        url  = "https://registry.yarnpkg.com/minimist/-/minimist-0.0.8.tgz";
        sha1 = "857fcabfc3397d2625b8228262e86aa7a011b05d";
      };
    }

    {
      name = "minimist-1.2.0.tgz";
      path = fetchurl {
        name = "minimist-1.2.0.tgz";
        url  = "https://registry.yarnpkg.com/minimist/-/minimist-1.2.0.tgz";
        sha1 = "a35008b20f41383eec1fb914f4cd5df79a264284";
      };
    }

    {
      name = "mississippi-3.0.0.tgz";
      path = fetchurl {
        name = "mississippi-3.0.0.tgz";
        url  = "https://registry.yarnpkg.com/mississippi/-/mississippi-3.0.0.tgz";
        sha1 = "ea0a3291f97e0b5e8776b363d5f0a12d94c67022";
      };
    }

    {
      name = "mixin-deep-1.3.2.tgz";
      path = fetchurl {
        name = "mixin-deep-1.3.2.tgz";
        url  = "https://registry.yarnpkg.com/mixin-deep/-/mixin-deep-1.3.2.tgz";
        sha1 = "1120b43dc359a785dce65b55b82e257ccf479566";
      };
    }

    {
      name = "mkdirp-0.5.1.tgz";
      path = fetchurl {
        name = "mkdirp-0.5.1.tgz";
        url  = "https://registry.yarnpkg.com/mkdirp/-/mkdirp-0.5.1.tgz";
        sha1 = "30057438eac6cf7f8c4767f38648d6697d75c903";
      };
    }

    {
      name = "move-concurrently-1.0.1.tgz";
      path = fetchurl {
        name = "move-concurrently-1.0.1.tgz";
        url  = "https://registry.yarnpkg.com/move-concurrently/-/move-concurrently-1.0.1.tgz";
        sha1 = "be2c005fda32e0b29af1f05d7c4b33214c701f92";
      };
    }

    {
      name = "ms-2.0.0.tgz";
      path = fetchurl {
        name = "ms-2.0.0.tgz";
        url  = "https://registry.yarnpkg.com/ms/-/ms-2.0.0.tgz";
        sha1 = "5608aeadfc00be6c2901df5f9861788de0d597c8";
      };
    }

    {
      name = "ms-2.1.1.tgz";
      path = fetchurl {
        name = "ms-2.1.1.tgz";
        url  = "https://registry.yarnpkg.com/ms/-/ms-2.1.1.tgz";
        sha1 = "30a5864eb3ebb0a66f2ebe6d727af06a09d86e0a";
      };
    }

    {
      name = "ms-2.1.2.tgz";
      path = fetchurl {
        name = "ms-2.1.2.tgz";
        url  = "https://registry.yarnpkg.com/ms/-/ms-2.1.2.tgz";
        sha1 = "d09d1f357b443f493382a8eb3ccd183872ae6009";
      };
    }

    {
      name = "multicast-dns-service-types-1.1.0.tgz";
      path = fetchurl {
        name = "multicast-dns-service-types-1.1.0.tgz";
        url  = "https://registry.yarnpkg.com/multicast-dns-service-types/-/multicast-dns-service-types-1.1.0.tgz";
        sha1 = "899f11d9686e5e05cb91b35d5f0e63b773cfc901";
      };
    }

    {
      name = "multicast-dns-6.2.3.tgz";
      path = fetchurl {
        name = "multicast-dns-6.2.3.tgz";
        url  = "https://registry.yarnpkg.com/multicast-dns/-/multicast-dns-6.2.3.tgz";
        sha1 = "a0ec7bd9055c4282f790c3c82f4e28db3b31b229";
      };
    }

    {
      name = "nan-2.14.0.tgz";
      path = fetchurl {
        name = "nan-2.14.0.tgz";
        url  = "https://registry.yarnpkg.com/nan/-/nan-2.14.0.tgz";
        sha1 = "7818f722027b2459a86f0295d434d1fc2336c52c";
      };
    }

    {
      name = "nanomatch-1.2.13.tgz";
      path = fetchurl {
        name = "nanomatch-1.2.13.tgz";
        url  = "https://registry.yarnpkg.com/nanomatch/-/nanomatch-1.2.13.tgz";
        sha1 = "b87a8aa4fc0de8fe6be88895b38983ff265bd119";
      };
    }

    {
      name = "negotiator-0.6.2.tgz";
      path = fetchurl {
        name = "negotiator-0.6.2.tgz";
        url  = "https://registry.yarnpkg.com/negotiator/-/negotiator-0.6.2.tgz";
        sha1 = "feacf7ccf525a77ae9634436a64883ffeca346fb";
      };
    }

    {
      name = "neo-async-2.6.1.tgz";
      path = fetchurl {
        name = "neo-async-2.6.1.tgz";
        url  = "https://registry.yarnpkg.com/neo-async/-/neo-async-2.6.1.tgz";
        sha1 = "ac27ada66167fa8849a6addd837f6b189ad2081c";
      };
    }

    {
      name = "nice-try-1.0.5.tgz";
      path = fetchurl {
        name = "nice-try-1.0.5.tgz";
        url  = "https://registry.yarnpkg.com/nice-try/-/nice-try-1.0.5.tgz";
        sha1 = "a3378a7696ce7d223e88fc9b764bd7ef1089e366";
      };
    }

    {
      name = "no-case-2.3.2.tgz";
      path = fetchurl {
        name = "no-case-2.3.2.tgz";
        url  = "https://registry.yarnpkg.com/no-case/-/no-case-2.3.2.tgz";
        sha1 = "60b813396be39b3f1288a4c1ed5d1e7d28b464ac";
      };
    }

    {
      name = "node-forge-0.9.0.tgz";
      path = fetchurl {
        name = "node-forge-0.9.0.tgz";
        url  = "https://registry.yarnpkg.com/node-forge/-/node-forge-0.9.0.tgz";
        sha1 = "d624050edbb44874adca12bb9a52ec63cb782579";
      };
    }

    {
      name = "node-gyp-3.8.0.tgz";
      path = fetchurl {
        name = "node-gyp-3.8.0.tgz";
        url  = "https://registry.yarnpkg.com/node-gyp/-/node-gyp-3.8.0.tgz";
        sha1 = "540304261c330e80d0d5edce253a68cb3964218c";
      };
    }

    {
      name = "node-libs-browser-2.2.1.tgz";
      path = fetchurl {
        name = "node-libs-browser-2.2.1.tgz";
        url  = "https://registry.yarnpkg.com/node-libs-browser/-/node-libs-browser-2.2.1.tgz";
        sha1 = "b64f513d18338625f90346d27b0d235e631f6425";
      };
    }

    {
      name = "node-sass-4.13.1.tgz";
      path = fetchurl {
        name = "node-sass-4.13.1.tgz";
        url  = "https://registry.yarnpkg.com/node-sass/-/node-sass-4.13.1.tgz";
        sha1 = "9db5689696bb2eec2c32b98bfea4c7a2e992d0a3";
      };
    }

    {
      name = "nopt-3.0.6.tgz";
      path = fetchurl {
        name = "nopt-3.0.6.tgz";
        url  = "https://registry.yarnpkg.com/nopt/-/nopt-3.0.6.tgz";
        sha1 = "c6465dbf08abcd4db359317f79ac68a646b28ff9";
      };
    }

    {
      name = "normalize-package-data-2.5.0.tgz";
      path = fetchurl {
        name = "normalize-package-data-2.5.0.tgz";
        url  = "https://registry.yarnpkg.com/normalize-package-data/-/normalize-package-data-2.5.0.tgz";
        sha1 = "e66db1838b200c1dfc233225d12cb36520e234a8";
      };
    }

    {
      name = "normalize-path-2.1.1.tgz";
      path = fetchurl {
        name = "normalize-path-2.1.1.tgz";
        url  = "https://registry.yarnpkg.com/normalize-path/-/normalize-path-2.1.1.tgz";
        sha1 = "1ab28b556e198363a8c1a6f7e6fa20137fe6aed9";
      };
    }

    {
      name = "normalize-path-3.0.0.tgz";
      path = fetchurl {
        name = "normalize-path-3.0.0.tgz";
        url  = "https://registry.yarnpkg.com/normalize-path/-/normalize-path-3.0.0.tgz";
        sha1 = "0dcd69ff23a1c9b11fd0978316644a0388216a65";
      };
    }

    {
      name = "npm-run-path-2.0.2.tgz";
      path = fetchurl {
        name = "npm-run-path-2.0.2.tgz";
        url  = "https://registry.yarnpkg.com/npm-run-path/-/npm-run-path-2.0.2.tgz";
        sha1 = "35a9232dfa35d7067b4cb2ddf2357b1871536c5f";
      };
    }

    {
      name = "npmlog-4.1.2.tgz";
      path = fetchurl {
        name = "npmlog-4.1.2.tgz";
        url  = "https://registry.yarnpkg.com/npmlog/-/npmlog-4.1.2.tgz";
        sha1 = "08a7f2a8bf734604779a9efa4ad5cc717abb954b";
      };
    }

    {
      name = "nth-check-1.0.2.tgz";
      path = fetchurl {
        name = "nth-check-1.0.2.tgz";
        url  = "https://registry.yarnpkg.com/nth-check/-/nth-check-1.0.2.tgz";
        sha1 = "b2bd295c37e3dd58a3bf0700376663ba4d9cf05c";
      };
    }

    {
      name = "number-is-nan-1.0.1.tgz";
      path = fetchurl {
        name = "number-is-nan-1.0.1.tgz";
        url  = "https://registry.yarnpkg.com/number-is-nan/-/number-is-nan-1.0.1.tgz";
        sha1 = "097b602b53422a522c1afb8790318336941a011d";
      };
    }

    {
      name = "oauth-sign-0.9.0.tgz";
      path = fetchurl {
        name = "oauth-sign-0.9.0.tgz";
        url  = "https://registry.yarnpkg.com/oauth-sign/-/oauth-sign-0.9.0.tgz";
        sha1 = "47a7b016baa68b5fa0ecf3dee08a85c679ac6455";
      };
    }

    {
      name = "object-assign-4.1.1.tgz";
      path = fetchurl {
        name = "object-assign-4.1.1.tgz";
        url  = "https://registry.yarnpkg.com/object-assign/-/object-assign-4.1.1.tgz";
        sha1 = "2109adc7965887cfc05cbbd442cac8bfbb360863";
      };
    }

    {
      name = "object-copy-0.1.0.tgz";
      path = fetchurl {
        name = "object-copy-0.1.0.tgz";
        url  = "https://registry.yarnpkg.com/object-copy/-/object-copy-0.1.0.tgz";
        sha1 = "7e7d858b781bd7c991a41ba975ed3812754e998c";
      };
    }

    {
      name = "object-inspect-1.7.0.tgz";
      path = fetchurl {
        name = "object-inspect-1.7.0.tgz";
        url  = "https://registry.yarnpkg.com/object-inspect/-/object-inspect-1.7.0.tgz";
        sha1 = "f4f6bd181ad77f006b5ece60bd0b6f398ff74a67";
      };
    }

    {
      name = "object-is-1.0.2.tgz";
      path = fetchurl {
        name = "object-is-1.0.2.tgz";
        url  = "https://registry.yarnpkg.com/object-is/-/object-is-1.0.2.tgz";
        sha1 = "6b80eb84fe451498f65007982f035a5b445edec4";
      };
    }

    {
      name = "object-keys-1.1.1.tgz";
      path = fetchurl {
        name = "object-keys-1.1.1.tgz";
        url  = "https://registry.yarnpkg.com/object-keys/-/object-keys-1.1.1.tgz";
        sha1 = "1c47f272df277f3b1daf061677d9c82e2322c60e";
      };
    }

    {
      name = "object-visit-1.0.1.tgz";
      path = fetchurl {
        name = "object-visit-1.0.1.tgz";
        url  = "https://registry.yarnpkg.com/object-visit/-/object-visit-1.0.1.tgz";
        sha1 = "f79c4493af0c5377b59fe39d395e41042dd045bb";
      };
    }

    {
      name = "object.assign-4.1.0.tgz";
      path = fetchurl {
        name = "object.assign-4.1.0.tgz";
        url  = "https://registry.yarnpkg.com/object.assign/-/object.assign-4.1.0.tgz";
        sha1 = "968bf1100d7956bb3ca086f006f846b3bc4008da";
      };
    }

    {
      name = "object.getownpropertydescriptors-2.1.0.tgz";
      path = fetchurl {
        name = "object.getownpropertydescriptors-2.1.0.tgz";
        url  = "https://registry.yarnpkg.com/object.getownpropertydescriptors/-/object.getownpropertydescriptors-2.1.0.tgz";
        sha1 = "369bf1f9592d8ab89d712dced5cb81c7c5352649";
      };
    }

    {
      name = "object.pick-1.3.0.tgz";
      path = fetchurl {
        name = "object.pick-1.3.0.tgz";
        url  = "https://registry.yarnpkg.com/object.pick/-/object.pick-1.3.0.tgz";
        sha1 = "87a10ac4c1694bd2e1cbf53591a66141fb5dd747";
      };
    }

    {
      name = "obuf-1.1.2.tgz";
      path = fetchurl {
        name = "obuf-1.1.2.tgz";
        url  = "https://registry.yarnpkg.com/obuf/-/obuf-1.1.2.tgz";
        sha1 = "09bea3343d41859ebd446292d11c9d4db619084e";
      };
    }

    {
      name = "on-finished-2.3.0.tgz";
      path = fetchurl {
        name = "on-finished-2.3.0.tgz";
        url  = "https://registry.yarnpkg.com/on-finished/-/on-finished-2.3.0.tgz";
        sha1 = "20f1336481b083cd75337992a16971aa2d906947";
      };
    }

    {
      name = "on-headers-1.0.2.tgz";
      path = fetchurl {
        name = "on-headers-1.0.2.tgz";
        url  = "https://registry.yarnpkg.com/on-headers/-/on-headers-1.0.2.tgz";
        sha1 = "772b0ae6aaa525c399e489adfad90c403eb3c28f";
      };
    }

    {
      name = "once-1.4.0.tgz";
      path = fetchurl {
        name = "once-1.4.0.tgz";
        url  = "https://registry.yarnpkg.com/once/-/once-1.4.0.tgz";
        sha1 = "583b1aa775961d4b113ac17d9c50baef9dd76bd1";
      };
    }

    {
      name = "opn-5.5.0.tgz";
      path = fetchurl {
        name = "opn-5.5.0.tgz";
        url  = "https://registry.yarnpkg.com/opn/-/opn-5.5.0.tgz";
        sha1 = "fc7164fab56d235904c51c3b27da6758ca3b9bfc";
      };
    }

    {
      name = "original-1.0.2.tgz";
      path = fetchurl {
        name = "original-1.0.2.tgz";
        url  = "https://registry.yarnpkg.com/original/-/original-1.0.2.tgz";
        sha1 = "e442a61cffe1c5fd20a65f3261c26663b303f25f";
      };
    }

    {
      name = "os-browserify-0.3.0.tgz";
      path = fetchurl {
        name = "os-browserify-0.3.0.tgz";
        url  = "https://registry.yarnpkg.com/os-browserify/-/os-browserify-0.3.0.tgz";
        sha1 = "854373c7f5c2315914fc9bfc6bd8238fdda1ec27";
      };
    }

    {
      name = "os-homedir-1.0.2.tgz";
      path = fetchurl {
        name = "os-homedir-1.0.2.tgz";
        url  = "https://registry.yarnpkg.com/os-homedir/-/os-homedir-1.0.2.tgz";
        sha1 = "ffbc4988336e0e833de0c168c7ef152121aa7fb3";
      };
    }

    {
      name = "os-locale-1.4.0.tgz";
      path = fetchurl {
        name = "os-locale-1.4.0.tgz";
        url  = "https://registry.yarnpkg.com/os-locale/-/os-locale-1.4.0.tgz";
        sha1 = "20f9f17ae29ed345e8bde583b13d2009803c14d9";
      };
    }

    {
      name = "os-locale-3.1.0.tgz";
      path = fetchurl {
        name = "os-locale-3.1.0.tgz";
        url  = "https://registry.yarnpkg.com/os-locale/-/os-locale-3.1.0.tgz";
        sha1 = "a802a6ee17f24c10483ab9935719cef4ed16bf1a";
      };
    }

    {
      name = "os-tmpdir-1.0.2.tgz";
      path = fetchurl {
        name = "os-tmpdir-1.0.2.tgz";
        url  = "https://registry.yarnpkg.com/os-tmpdir/-/os-tmpdir-1.0.2.tgz";
        sha1 = "bbe67406c79aa85c5cfec766fe5734555dfa1274";
      };
    }

    {
      name = "osenv-0.1.5.tgz";
      path = fetchurl {
        name = "osenv-0.1.5.tgz";
        url  = "https://registry.yarnpkg.com/osenv/-/osenv-0.1.5.tgz";
        sha1 = "85cdfafaeb28e8677f416e287592b5f3f49ea410";
      };
    }

    {
      name = "p-defer-1.0.0.tgz";
      path = fetchurl {
        name = "p-defer-1.0.0.tgz";
        url  = "https://registry.yarnpkg.com/p-defer/-/p-defer-1.0.0.tgz";
        sha1 = "9f6eb182f6c9aa8cd743004a7d4f96b196b0fb0c";
      };
    }

    {
      name = "p-finally-1.0.0.tgz";
      path = fetchurl {
        name = "p-finally-1.0.0.tgz";
        url  = "https://registry.yarnpkg.com/p-finally/-/p-finally-1.0.0.tgz";
        sha1 = "3fbcfb15b899a44123b34b6dcc18b724336a2cae";
      };
    }

    {
      name = "p-is-promise-2.1.0.tgz";
      path = fetchurl {
        name = "p-is-promise-2.1.0.tgz";
        url  = "https://registry.yarnpkg.com/p-is-promise/-/p-is-promise-2.1.0.tgz";
        sha1 = "918cebaea248a62cf7ffab8e3bca8c5f882fc42e";
      };
    }

    {
      name = "p-limit-2.2.2.tgz";
      path = fetchurl {
        name = "p-limit-2.2.2.tgz";
        url  = "https://registry.yarnpkg.com/p-limit/-/p-limit-2.2.2.tgz";
        sha1 = "61279b67721f5287aa1c13a9a7fbbc48c9291b1e";
      };
    }

    {
      name = "p-locate-3.0.0.tgz";
      path = fetchurl {
        name = "p-locate-3.0.0.tgz";
        url  = "https://registry.yarnpkg.com/p-locate/-/p-locate-3.0.0.tgz";
        sha1 = "322d69a05c0264b25997d9f40cd8a891ab0064a4";
      };
    }

    {
      name = "p-map-2.1.0.tgz";
      path = fetchurl {
        name = "p-map-2.1.0.tgz";
        url  = "https://registry.yarnpkg.com/p-map/-/p-map-2.1.0.tgz";
        sha1 = "310928feef9c9ecc65b68b17693018a665cea175";
      };
    }

    {
      name = "p-retry-3.0.1.tgz";
      path = fetchurl {
        name = "p-retry-3.0.1.tgz";
        url  = "https://registry.yarnpkg.com/p-retry/-/p-retry-3.0.1.tgz";
        sha1 = "316b4c8893e2c8dc1cfa891f406c4b422bebf328";
      };
    }

    {
      name = "p-try-2.2.0.tgz";
      path = fetchurl {
        name = "p-try-2.2.0.tgz";
        url  = "https://registry.yarnpkg.com/p-try/-/p-try-2.2.0.tgz";
        sha1 = "cb2868540e313d61de58fafbe35ce9004d5540e6";
      };
    }

    {
      name = "pako-1.0.11.tgz";
      path = fetchurl {
        name = "pako-1.0.11.tgz";
        url  = "https://registry.yarnpkg.com/pako/-/pako-1.0.11.tgz";
        sha1 = "6c9599d340d54dfd3946380252a35705a6b992bf";
      };
    }

    {
      name = "parallel-transform-1.2.0.tgz";
      path = fetchurl {
        name = "parallel-transform-1.2.0.tgz";
        url  = "https://registry.yarnpkg.com/parallel-transform/-/parallel-transform-1.2.0.tgz";
        sha1 = "9049ca37d6cb2182c3b1d2c720be94d14a5814fc";
      };
    }

    {
      name = "param-case-2.1.1.tgz";
      path = fetchurl {
        name = "param-case-2.1.1.tgz";
        url  = "https://registry.yarnpkg.com/param-case/-/param-case-2.1.1.tgz";
        sha1 = "df94fd8cf6531ecf75e6bef9a0858fbc72be2247";
      };
    }

    {
      name = "parse-asn1-5.1.5.tgz";
      path = fetchurl {
        name = "parse-asn1-5.1.5.tgz";
        url  = "https://registry.yarnpkg.com/parse-asn1/-/parse-asn1-5.1.5.tgz";
        sha1 = "003271343da58dc94cace494faef3d2147ecea0e";
      };
    }

    {
      name = "parse-json-2.2.0.tgz";
      path = fetchurl {
        name = "parse-json-2.2.0.tgz";
        url  = "https://registry.yarnpkg.com/parse-json/-/parse-json-2.2.0.tgz";
        sha1 = "f480f40434ef80741f8469099f8dea18f55a4dc9";
      };
    }

    {
      name = "parse-passwd-1.0.0.tgz";
      path = fetchurl {
        name = "parse-passwd-1.0.0.tgz";
        url  = "https://registry.yarnpkg.com/parse-passwd/-/parse-passwd-1.0.0.tgz";
        sha1 = "6d5b934a456993b23d37f40a382d6f1666a8e5c6";
      };
    }

    {
      name = "parseurl-1.3.3.tgz";
      path = fetchurl {
        name = "parseurl-1.3.3.tgz";
        url  = "https://registry.yarnpkg.com/parseurl/-/parseurl-1.3.3.tgz";
        sha1 = "9da19e7bee8d12dff0513ed5b76957793bc2e8d4";
      };
    }

    {
      name = "pascalcase-0.1.1.tgz";
      path = fetchurl {
        name = "pascalcase-0.1.1.tgz";
        url  = "https://registry.yarnpkg.com/pascalcase/-/pascalcase-0.1.1.tgz";
        sha1 = "b363e55e8006ca6fe21784d2db22bd15d7917f14";
      };
    }

    {
      name = "path-browserify-0.0.1.tgz";
      path = fetchurl {
        name = "path-browserify-0.0.1.tgz";
        url  = "https://registry.yarnpkg.com/path-browserify/-/path-browserify-0.0.1.tgz";
        sha1 = "e6c4ddd7ed3aa27c68a20cc4e50e1a4ee83bbc4a";
      };
    }

    {
      name = "path-dirname-1.0.2.tgz";
      path = fetchurl {
        name = "path-dirname-1.0.2.tgz";
        url  = "https://registry.yarnpkg.com/path-dirname/-/path-dirname-1.0.2.tgz";
        sha1 = "cc33d24d525e099a5388c0336c6e32b9160609e0";
      };
    }

    {
      name = "path-exists-2.1.0.tgz";
      path = fetchurl {
        name = "path-exists-2.1.0.tgz";
        url  = "https://registry.yarnpkg.com/path-exists/-/path-exists-2.1.0.tgz";
        sha1 = "0feb6c64f0fc518d9a754dd5efb62c7022761f4b";
      };
    }

    {
      name = "path-exists-3.0.0.tgz";
      path = fetchurl {
        name = "path-exists-3.0.0.tgz";
        url  = "https://registry.yarnpkg.com/path-exists/-/path-exists-3.0.0.tgz";
        sha1 = "ce0ebeaa5f78cb18925ea7d810d7b59b010fd515";
      };
    }

    {
      name = "path-is-absolute-1.0.1.tgz";
      path = fetchurl {
        name = "path-is-absolute-1.0.1.tgz";
        url  = "https://registry.yarnpkg.com/path-is-absolute/-/path-is-absolute-1.0.1.tgz";
        sha1 = "174b9268735534ffbc7ace6bf53a5a9e1b5c5f5f";
      };
    }

    {
      name = "path-is-inside-1.0.2.tgz";
      path = fetchurl {
        name = "path-is-inside-1.0.2.tgz";
        url  = "https://registry.yarnpkg.com/path-is-inside/-/path-is-inside-1.0.2.tgz";
        sha1 = "365417dede44430d1c11af61027facf074bdfc53";
      };
    }

    {
      name = "path-key-2.0.1.tgz";
      path = fetchurl {
        name = "path-key-2.0.1.tgz";
        url  = "https://registry.yarnpkg.com/path-key/-/path-key-2.0.1.tgz";
        sha1 = "411cadb574c5a140d3a4b1910d40d80cc9f40b40";
      };
    }

    {
      name = "path-parse-1.0.6.tgz";
      path = fetchurl {
        name = "path-parse-1.0.6.tgz";
        url  = "https://registry.yarnpkg.com/path-parse/-/path-parse-1.0.6.tgz";
        sha1 = "d62dbb5679405d72c4737ec58600e9ddcf06d24c";
      };
    }

    {
      name = "path-to-regexp-0.1.7.tgz";
      path = fetchurl {
        name = "path-to-regexp-0.1.7.tgz";
        url  = "https://registry.yarnpkg.com/path-to-regexp/-/path-to-regexp-0.1.7.tgz";
        sha1 = "df604178005f522f15eb4490e7247a1bfaa67f8c";
      };
    }

    {
      name = "path-type-1.1.0.tgz";
      path = fetchurl {
        name = "path-type-1.1.0.tgz";
        url  = "https://registry.yarnpkg.com/path-type/-/path-type-1.1.0.tgz";
        sha1 = "59c44f7ee491da704da415da5a4070ba4f8fe441";
      };
    }

    {
      name = "pbkdf2-3.0.17.tgz";
      path = fetchurl {
        name = "pbkdf2-3.0.17.tgz";
        url  = "https://registry.yarnpkg.com/pbkdf2/-/pbkdf2-3.0.17.tgz";
        sha1 = "976c206530617b14ebb32114239f7b09336e93a6";
      };
    }

    {
      name = "performance-now-2.1.0.tgz";
      path = fetchurl {
        name = "performance-now-2.1.0.tgz";
        url  = "https://registry.yarnpkg.com/performance-now/-/performance-now-2.1.0.tgz";
        sha1 = "6309f4e0e5fa913ec1c69307ae364b4b377c9e7b";
      };
    }

    {
      name = "pify-2.3.0.tgz";
      path = fetchurl {
        name = "pify-2.3.0.tgz";
        url  = "https://registry.yarnpkg.com/pify/-/pify-2.3.0.tgz";
        sha1 = "ed141a6ac043a849ea588498e7dca8b15330e90c";
      };
    }

    {
      name = "pify-4.0.1.tgz";
      path = fetchurl {
        name = "pify-4.0.1.tgz";
        url  = "https://registry.yarnpkg.com/pify/-/pify-4.0.1.tgz";
        sha1 = "4b2cd25c50d598735c50292224fd8c6df41e3231";
      };
    }

    {
      name = "pinkie-promise-2.0.1.tgz";
      path = fetchurl {
        name = "pinkie-promise-2.0.1.tgz";
        url  = "https://registry.yarnpkg.com/pinkie-promise/-/pinkie-promise-2.0.1.tgz";
        sha1 = "2135d6dfa7a358c069ac9b178776288228450ffa";
      };
    }

    {
      name = "pinkie-2.0.4.tgz";
      path = fetchurl {
        name = "pinkie-2.0.4.tgz";
        url  = "https://registry.yarnpkg.com/pinkie/-/pinkie-2.0.4.tgz";
        sha1 = "72556b80cfa0d48a974e80e77248e80ed4f7f870";
      };
    }

    {
      name = "pkg-dir-3.0.0.tgz";
      path = fetchurl {
        name = "pkg-dir-3.0.0.tgz";
        url  = "https://registry.yarnpkg.com/pkg-dir/-/pkg-dir-3.0.0.tgz";
        sha1 = "2749020f239ed990881b1f71210d51eb6523bea3";
      };
    }

    {
      name = "popper.js-1.16.1.tgz";
      path = fetchurl {
        name = "popper.js-1.16.1.tgz";
        url  = "https://registry.yarnpkg.com/popper.js/-/popper.js-1.16.1.tgz";
        sha1 = "2a223cb3dc7b6213d740e40372be40de43e65b1b";
      };
    }

    {
      name = "portfinder-1.0.25.tgz";
      path = fetchurl {
        name = "portfinder-1.0.25.tgz";
        url  = "https://registry.yarnpkg.com/portfinder/-/portfinder-1.0.25.tgz";
        sha1 = "254fd337ffba869f4b9d37edc298059cb4d35eca";
      };
    }

    {
      name = "posix-character-classes-0.1.1.tgz";
      path = fetchurl {
        name = "posix-character-classes-0.1.1.tgz";
        url  = "https://registry.yarnpkg.com/posix-character-classes/-/posix-character-classes-0.1.1.tgz";
        sha1 = "01eac0fe3b5af71a2a6c02feabb8c1fef7e00eab";
      };
    }

    {
      name = "postcss-modules-extract-imports-1.2.1.tgz";
      path = fetchurl {
        name = "postcss-modules-extract-imports-1.2.1.tgz";
        url  = "https://registry.yarnpkg.com/postcss-modules-extract-imports/-/postcss-modules-extract-imports-1.2.1.tgz";
        sha1 = "dc87e34148ec7eab5f791f7cd5849833375b741a";
      };
    }

    {
      name = "postcss-modules-local-by-default-1.2.0.tgz";
      path = fetchurl {
        name = "postcss-modules-local-by-default-1.2.0.tgz";
        url  = "https://registry.yarnpkg.com/postcss-modules-local-by-default/-/postcss-modules-local-by-default-1.2.0.tgz";
        sha1 = "f7d80c398c5a393fa7964466bd19500a7d61c069";
      };
    }

    {
      name = "postcss-modules-scope-1.1.0.tgz";
      path = fetchurl {
        name = "postcss-modules-scope-1.1.0.tgz";
        url  = "https://registry.yarnpkg.com/postcss-modules-scope/-/postcss-modules-scope-1.1.0.tgz";
        sha1 = "d6ea64994c79f97b62a72b426fbe6056a194bb90";
      };
    }

    {
      name = "postcss-modules-values-1.3.0.tgz";
      path = fetchurl {
        name = "postcss-modules-values-1.3.0.tgz";
        url  = "https://registry.yarnpkg.com/postcss-modules-values/-/postcss-modules-values-1.3.0.tgz";
        sha1 = "ecffa9d7e192518389f42ad0e83f72aec456ea20";
      };
    }

    {
      name = "postcss-value-parser-3.3.1.tgz";
      path = fetchurl {
        name = "postcss-value-parser-3.3.1.tgz";
        url  = "https://registry.yarnpkg.com/postcss-value-parser/-/postcss-value-parser-3.3.1.tgz";
        sha1 = "9ff822547e2893213cf1c30efa51ac5fd1ba8281";
      };
    }

    {
      name = "postcss-6.0.23.tgz";
      path = fetchurl {
        name = "postcss-6.0.23.tgz";
        url  = "https://registry.yarnpkg.com/postcss/-/postcss-6.0.23.tgz";
        sha1 = "61c82cc328ac60e677645f979054eb98bc0e3324";
      };
    }

    {
      name = "pretty-error-2.1.1.tgz";
      path = fetchurl {
        name = "pretty-error-2.1.1.tgz";
        url  = "https://registry.yarnpkg.com/pretty-error/-/pretty-error-2.1.1.tgz";
        sha1 = "5f4f87c8f91e5ae3f3ba87ab4cf5e03b1a17f1a3";
      };
    }

    {
      name = "process-nextick-args-2.0.1.tgz";
      path = fetchurl {
        name = "process-nextick-args-2.0.1.tgz";
        url  = "https://registry.yarnpkg.com/process-nextick-args/-/process-nextick-args-2.0.1.tgz";
        sha1 = "7820d9b16120cc55ca9ae7792680ae7dba6d7fe2";
      };
    }

    {
      name = "process-0.11.10.tgz";
      path = fetchurl {
        name = "process-0.11.10.tgz";
        url  = "https://registry.yarnpkg.com/process/-/process-0.11.10.tgz";
        sha1 = "7332300e840161bda3e69a1d1d91a7d4bc16f182";
      };
    }

    {
      name = "promise-inflight-1.0.1.tgz";
      path = fetchurl {
        name = "promise-inflight-1.0.1.tgz";
        url  = "https://registry.yarnpkg.com/promise-inflight/-/promise-inflight-1.0.1.tgz";
        sha1 = "98472870bf228132fcbdd868129bad12c3c029e3";
      };
    }

    {
      name = "promise-retry-1.1.1.tgz";
      path = fetchurl {
        name = "promise-retry-1.1.1.tgz";
        url  = "https://registry.yarnpkg.com/promise-retry/-/promise-retry-1.1.1.tgz";
        sha1 = "6739e968e3051da20ce6497fb2b50f6911df3d6d";
      };
    }

    {
      name = "proxy-addr-2.0.5.tgz";
      path = fetchurl {
        name = "proxy-addr-2.0.5.tgz";
        url  = "https://registry.yarnpkg.com/proxy-addr/-/proxy-addr-2.0.5.tgz";
        sha1 = "34cbd64a2d81f4b1fd21e76f9f06c8a45299ee34";
      };
    }

    {
      name = "prr-1.0.1.tgz";
      path = fetchurl {
        name = "prr-1.0.1.tgz";
        url  = "https://registry.yarnpkg.com/prr/-/prr-1.0.1.tgz";
        sha1 = "d3fc114ba06995a45ec6893f484ceb1d78f5f476";
      };
    }

    {
      name = "pseudomap-1.0.2.tgz";
      path = fetchurl {
        name = "pseudomap-1.0.2.tgz";
        url  = "https://registry.yarnpkg.com/pseudomap/-/pseudomap-1.0.2.tgz";
        sha1 = "f052a28da70e618917ef0a8ac34c1ae5a68286b3";
      };
    }

    {
      name = "psl-1.7.0.tgz";
      path = fetchurl {
        name = "psl-1.7.0.tgz";
        url  = "https://registry.yarnpkg.com/psl/-/psl-1.7.0.tgz";
        sha1 = "f1c4c47a8ef97167dea5d6bbf4816d736e884a3c";
      };
    }

    {
      name = "public-encrypt-4.0.3.tgz";
      path = fetchurl {
        name = "public-encrypt-4.0.3.tgz";
        url  = "https://registry.yarnpkg.com/public-encrypt/-/public-encrypt-4.0.3.tgz";
        sha1 = "4fcc9d77a07e48ba7527e7cbe0de33d0701331e0";
      };
    }

    {
      name = "pump-2.0.1.tgz";
      path = fetchurl {
        name = "pump-2.0.1.tgz";
        url  = "https://registry.yarnpkg.com/pump/-/pump-2.0.1.tgz";
        sha1 = "12399add6e4cf7526d973cbc8b5ce2e2908b3909";
      };
    }

    {
      name = "pump-3.0.0.tgz";
      path = fetchurl {
        name = "pump-3.0.0.tgz";
        url  = "https://registry.yarnpkg.com/pump/-/pump-3.0.0.tgz";
        sha1 = "b4a2116815bde2f4e1ea602354e8c75565107a64";
      };
    }

    {
      name = "pumpify-1.5.1.tgz";
      path = fetchurl {
        name = "pumpify-1.5.1.tgz";
        url  = "https://registry.yarnpkg.com/pumpify/-/pumpify-1.5.1.tgz";
        sha1 = "36513be246ab27570b1a374a5ce278bfd74370ce";
      };
    }

    {
      name = "punycode-1.3.2.tgz";
      path = fetchurl {
        name = "punycode-1.3.2.tgz";
        url  = "https://registry.yarnpkg.com/punycode/-/punycode-1.3.2.tgz";
        sha1 = "9653a036fb7c1ee42342f2325cceefea3926c48d";
      };
    }

    {
      name = "punycode-1.4.1.tgz";
      path = fetchurl {
        name = "punycode-1.4.1.tgz";
        url  = "https://registry.yarnpkg.com/punycode/-/punycode-1.4.1.tgz";
        sha1 = "c0d5a63b2718800ad8e1eb0fa5269c84dd41845e";
      };
    }

    {
      name = "punycode-2.1.1.tgz";
      path = fetchurl {
        name = "punycode-2.1.1.tgz";
        url  = "https://registry.yarnpkg.com/punycode/-/punycode-2.1.1.tgz";
        sha1 = "b58b010ac40c22c5657616c8d2c2c02c7bf479ec";
      };
    }

    {
      name = "purs-loader-3.7.1.tgz";
      path = fetchurl {
        name = "purs-loader-3.7.1.tgz";
        url  = "https://registry.yarnpkg.com/purs-loader/-/purs-loader-3.7.1.tgz";
        sha1 = "190ae0362f03648bd0200fda436eeb0aeacb2616";
      };
    }

    {
      name = "qs-6.7.0.tgz";
      path = fetchurl {
        name = "qs-6.7.0.tgz";
        url  = "https://registry.yarnpkg.com/qs/-/qs-6.7.0.tgz";
        sha1 = "41dc1a015e3d581f1621776be31afb2876a9b1bc";
      };
    }

    {
      name = "qs-6.5.2.tgz";
      path = fetchurl {
        name = "qs-6.5.2.tgz";
        url  = "https://registry.yarnpkg.com/qs/-/qs-6.5.2.tgz";
        sha1 = "cb3ae806e8740444584ef154ce8ee98d403f3e36";
      };
    }

    {
      name = "querystring-es3-0.2.1.tgz";
      path = fetchurl {
        name = "querystring-es3-0.2.1.tgz";
        url  = "https://registry.yarnpkg.com/querystring-es3/-/querystring-es3-0.2.1.tgz";
        sha1 = "9ec61f79049875707d69414596fd907a4d711e73";
      };
    }

    {
      name = "querystring-0.2.0.tgz";
      path = fetchurl {
        name = "querystring-0.2.0.tgz";
        url  = "https://registry.yarnpkg.com/querystring/-/querystring-0.2.0.tgz";
        sha1 = "b209849203bb25df820da756e747005878521620";
      };
    }

    {
      name = "querystringify-2.1.1.tgz";
      path = fetchurl {
        name = "querystringify-2.1.1.tgz";
        url  = "https://registry.yarnpkg.com/querystringify/-/querystringify-2.1.1.tgz";
        sha1 = "60e5a5fd64a7f8bfa4d2ab2ed6fdf4c85bad154e";
      };
    }

    {
      name = "randombytes-2.1.0.tgz";
      path = fetchurl {
        name = "randombytes-2.1.0.tgz";
        url  = "https://registry.yarnpkg.com/randombytes/-/randombytes-2.1.0.tgz";
        sha1 = "df6f84372f0270dc65cdf6291349ab7a473d4f2a";
      };
    }

    {
      name = "randomfill-1.0.4.tgz";
      path = fetchurl {
        name = "randomfill-1.0.4.tgz";
        url  = "https://registry.yarnpkg.com/randomfill/-/randomfill-1.0.4.tgz";
        sha1 = "c92196fc86ab42be983f1bf31778224931d61458";
      };
    }

    {
      name = "range-parser-1.2.1.tgz";
      path = fetchurl {
        name = "range-parser-1.2.1.tgz";
        url  = "https://registry.yarnpkg.com/range-parser/-/range-parser-1.2.1.tgz";
        sha1 = "3cf37023d199e1c24d1a55b84800c2f3e6468031";
      };
    }

    {
      name = "raw-body-2.4.0.tgz";
      path = fetchurl {
        name = "raw-body-2.4.0.tgz";
        url  = "https://registry.yarnpkg.com/raw-body/-/raw-body-2.4.0.tgz";
        sha1 = "a1ce6fb9c9bc356ca52e89256ab59059e13d0332";
      };
    }

    {
      name = "read-pkg-up-1.0.1.tgz";
      path = fetchurl {
        name = "read-pkg-up-1.0.1.tgz";
        url  = "https://registry.yarnpkg.com/read-pkg-up/-/read-pkg-up-1.0.1.tgz";
        sha1 = "9d63c13276c065918d57f002a57f40a1b643fb02";
      };
    }

    {
      name = "read-pkg-1.1.0.tgz";
      path = fetchurl {
        name = "read-pkg-1.1.0.tgz";
        url  = "https://registry.yarnpkg.com/read-pkg/-/read-pkg-1.1.0.tgz";
        sha1 = "f5ffaa5ecd29cb31c0474bca7d756b6bb29e3f28";
      };
    }

    {
      name = "readable-stream-2.3.7.tgz";
      path = fetchurl {
        name = "readable-stream-2.3.7.tgz";
        url  = "https://registry.yarnpkg.com/readable-stream/-/readable-stream-2.3.7.tgz";
        sha1 = "1eca1cf711aef814c04f62252a36a62f6cb23b57";
      };
    }

    {
      name = "readable-stream-3.6.0.tgz";
      path = fetchurl {
        name = "readable-stream-3.6.0.tgz";
        url  = "https://registry.yarnpkg.com/readable-stream/-/readable-stream-3.6.0.tgz";
        sha1 = "337bbda3adc0706bd3e024426a286d4b4b2c9198";
      };
    }

    {
      name = "readdirp-2.2.1.tgz";
      path = fetchurl {
        name = "readdirp-2.2.1.tgz";
        url  = "https://registry.yarnpkg.com/readdirp/-/readdirp-2.2.1.tgz";
        sha1 = "0e87622a3325aa33e892285caf8b4e846529a525";
      };
    }

    {
      name = "redent-1.0.0.tgz";
      path = fetchurl {
        name = "redent-1.0.0.tgz";
        url  = "https://registry.yarnpkg.com/redent/-/redent-1.0.0.tgz";
        sha1 = "cf916ab1fd5f1f16dfb20822dd6ec7f730c2afde";
      };
    }

    {
      name = "regenerate-1.4.0.tgz";
      path = fetchurl {
        name = "regenerate-1.4.0.tgz";
        url  = "https://registry.yarnpkg.com/regenerate/-/regenerate-1.4.0.tgz";
        sha1 = "4a856ec4b56e4077c557589cae85e7a4c8869a11";
      };
    }

    {
      name = "regex-not-1.0.2.tgz";
      path = fetchurl {
        name = "regex-not-1.0.2.tgz";
        url  = "https://registry.yarnpkg.com/regex-not/-/regex-not-1.0.2.tgz";
        sha1 = "1f4ece27e00b0b65e0247a6810e6a85d83a5752c";
      };
    }

    {
      name = "regexp.prototype.flags-1.3.0.tgz";
      path = fetchurl {
        name = "regexp.prototype.flags-1.3.0.tgz";
        url  = "https://registry.yarnpkg.com/regexp.prototype.flags/-/regexp.prototype.flags-1.3.0.tgz";
        sha1 = "7aba89b3c13a64509dabcf3ca8d9fbb9bdf5cb75";
      };
    }

    {
      name = "regexpu-core-1.0.0.tgz";
      path = fetchurl {
        name = "regexpu-core-1.0.0.tgz";
        url  = "https://registry.yarnpkg.com/regexpu-core/-/regexpu-core-1.0.0.tgz";
        sha1 = "86a763f58ee4d7c2f6b102e4764050de7ed90c6b";
      };
    }

    {
      name = "regjsgen-0.2.0.tgz";
      path = fetchurl {
        name = "regjsgen-0.2.0.tgz";
        url  = "https://registry.yarnpkg.com/regjsgen/-/regjsgen-0.2.0.tgz";
        sha1 = "6c016adeac554f75823fe37ac05b92d5a4edb1f7";
      };
    }

    {
      name = "regjsparser-0.1.5.tgz";
      path = fetchurl {
        name = "regjsparser-0.1.5.tgz";
        url  = "https://registry.yarnpkg.com/regjsparser/-/regjsparser-0.1.5.tgz";
        sha1 = "7ee8f84dc6fa792d3fd0ae228d24bd949ead205c";
      };
    }

    {
      name = "relateurl-0.2.7.tgz";
      path = fetchurl {
        name = "relateurl-0.2.7.tgz";
        url  = "https://registry.yarnpkg.com/relateurl/-/relateurl-0.2.7.tgz";
        sha1 = "54dbf377e51440aca90a4cd274600d3ff2d888a9";
      };
    }

    {
      name = "remove-trailing-separator-1.1.0.tgz";
      path = fetchurl {
        name = "remove-trailing-separator-1.1.0.tgz";
        url  = "https://registry.yarnpkg.com/remove-trailing-separator/-/remove-trailing-separator-1.1.0.tgz";
        sha1 = "c24bce2a283adad5bc3f58e0d48249b92379d8ef";
      };
    }

    {
      name = "renderkid-2.0.3.tgz";
      path = fetchurl {
        name = "renderkid-2.0.3.tgz";
        url  = "https://registry.yarnpkg.com/renderkid/-/renderkid-2.0.3.tgz";
        sha1 = "380179c2ff5ae1365c522bf2fcfcff01c5b74149";
      };
    }

    {
      name = "repeat-element-1.1.3.tgz";
      path = fetchurl {
        name = "repeat-element-1.1.3.tgz";
        url  = "https://registry.yarnpkg.com/repeat-element/-/repeat-element-1.1.3.tgz";
        sha1 = "782e0d825c0c5a3bb39731f84efee6b742e6b1ce";
      };
    }

    {
      name = "repeat-string-1.6.1.tgz";
      path = fetchurl {
        name = "repeat-string-1.6.1.tgz";
        url  = "https://registry.yarnpkg.com/repeat-string/-/repeat-string-1.6.1.tgz";
        sha1 = "8dcae470e1c88abc2d600fff4a776286da75e637";
      };
    }

    {
      name = "repeating-2.0.1.tgz";
      path = fetchurl {
        name = "repeating-2.0.1.tgz";
        url  = "https://registry.yarnpkg.com/repeating/-/repeating-2.0.1.tgz";
        sha1 = "5214c53a926d3552707527fbab415dbc08d06dda";
      };
    }

    {
      name = "request-2.88.2.tgz";
      path = fetchurl {
        name = "request-2.88.2.tgz";
        url  = "https://registry.yarnpkg.com/request/-/request-2.88.2.tgz";
        sha1 = "d73c918731cb5a87da047e207234146f664d12b3";
      };
    }

    {
      name = "require-directory-2.1.1.tgz";
      path = fetchurl {
        name = "require-directory-2.1.1.tgz";
        url  = "https://registry.yarnpkg.com/require-directory/-/require-directory-2.1.1.tgz";
        sha1 = "8c64ad5fd30dab1c976e2344ffe7f792a6a6df42";
      };
    }

    {
      name = "require-main-filename-1.0.1.tgz";
      path = fetchurl {
        name = "require-main-filename-1.0.1.tgz";
        url  = "https://registry.yarnpkg.com/require-main-filename/-/require-main-filename-1.0.1.tgz";
        sha1 = "97f717b69d48784f5f526a6c5aa8ffdda055a4d1";
      };
    }

    {
      name = "require-main-filename-2.0.0.tgz";
      path = fetchurl {
        name = "require-main-filename-2.0.0.tgz";
        url  = "https://registry.yarnpkg.com/require-main-filename/-/require-main-filename-2.0.0.tgz";
        sha1 = "d0b329ecc7cc0f61649f62215be69af54aa8989b";
      };
    }

    {
      name = "requires-port-1.0.0.tgz";
      path = fetchurl {
        name = "requires-port-1.0.0.tgz";
        url  = "https://registry.yarnpkg.com/requires-port/-/requires-port-1.0.0.tgz";
        sha1 = "925d2601d39ac485e091cf0da5c6e694dc3dcaff";
      };
    }

    {
      name = "resolve-cwd-2.0.0.tgz";
      path = fetchurl {
        name = "resolve-cwd-2.0.0.tgz";
        url  = "https://registry.yarnpkg.com/resolve-cwd/-/resolve-cwd-2.0.0.tgz";
        sha1 = "00a9f7387556e27038eae232caa372a6a59b665a";
      };
    }

    {
      name = "resolve-dir-1.0.1.tgz";
      path = fetchurl {
        name = "resolve-dir-1.0.1.tgz";
        url  = "https://registry.yarnpkg.com/resolve-dir/-/resolve-dir-1.0.1.tgz";
        sha1 = "79a40644c362be82f26effe739c9bb5382046f43";
      };
    }

    {
      name = "resolve-from-3.0.0.tgz";
      path = fetchurl {
        name = "resolve-from-3.0.0.tgz";
        url  = "https://registry.yarnpkg.com/resolve-from/-/resolve-from-3.0.0.tgz";
        sha1 = "b22c7af7d9d6881bc8b6e653335eebcb0a188748";
      };
    }

    {
      name = "resolve-url-0.2.1.tgz";
      path = fetchurl {
        name = "resolve-url-0.2.1.tgz";
        url  = "https://registry.yarnpkg.com/resolve-url/-/resolve-url-0.2.1.tgz";
        sha1 = "2c637fe77c893afd2a663fe21aa9080068e2052a";
      };
    }

    {
      name = "resolve-1.15.1.tgz";
      path = fetchurl {
        name = "resolve-1.15.1.tgz";
        url  = "https://registry.yarnpkg.com/resolve/-/resolve-1.15.1.tgz";
        sha1 = "27bdcdeffeaf2d6244b95bb0f9f4b4653451f3e8";
      };
    }

    {
      name = "ret-0.1.15.tgz";
      path = fetchurl {
        name = "ret-0.1.15.tgz";
        url  = "https://registry.yarnpkg.com/ret/-/ret-0.1.15.tgz";
        sha1 = "b8a4825d5bdb1fc3f6f53c2bc33f81388681c7bc";
      };
    }

    {
      name = "retry-0.10.1.tgz";
      path = fetchurl {
        name = "retry-0.10.1.tgz";
        url  = "https://registry.yarnpkg.com/retry/-/retry-0.10.1.tgz";
        sha1 = "e76388d217992c252750241d3d3956fed98d8ff4";
      };
    }

    {
      name = "retry-0.12.0.tgz";
      path = fetchurl {
        name = "retry-0.12.0.tgz";
        url  = "https://registry.yarnpkg.com/retry/-/retry-0.12.0.tgz";
        sha1 = "1b42a6266a21f07421d1b0b54b7dc167b01c013b";
      };
    }

    {
      name = "rimraf-2.7.1.tgz";
      path = fetchurl {
        name = "rimraf-2.7.1.tgz";
        url  = "https://registry.yarnpkg.com/rimraf/-/rimraf-2.7.1.tgz";
        sha1 = "35797f13a7fdadc566142c29d4f07ccad483e3ec";
      };
    }

    {
      name = "ripemd160-2.0.2.tgz";
      path = fetchurl {
        name = "ripemd160-2.0.2.tgz";
        url  = "https://registry.yarnpkg.com/ripemd160/-/ripemd160-2.0.2.tgz";
        sha1 = "a1c1a6f624751577ba5d07914cbc92850585890c";
      };
    }

    {
      name = "run-queue-1.0.3.tgz";
      path = fetchurl {
        name = "run-queue-1.0.3.tgz";
        url  = "https://registry.yarnpkg.com/run-queue/-/run-queue-1.0.3.tgz";
        sha1 = "e848396f057d223f24386924618e25694161ec47";
      };
    }

    {
      name = "safe-buffer-5.1.2.tgz";
      path = fetchurl {
        name = "safe-buffer-5.1.2.tgz";
        url  = "https://registry.yarnpkg.com/safe-buffer/-/safe-buffer-5.1.2.tgz";
        sha1 = "991ec69d296e0313747d59bdfd2b745c35f8828d";
      };
    }

    {
      name = "safe-buffer-5.2.0.tgz";
      path = fetchurl {
        name = "safe-buffer-5.2.0.tgz";
        url  = "https://registry.yarnpkg.com/safe-buffer/-/safe-buffer-5.2.0.tgz";
        sha1 = "b74daec49b1148f88c64b68d49b1e815c1f2f519";
      };
    }

    {
      name = "safe-regex-1.1.0.tgz";
      path = fetchurl {
        name = "safe-regex-1.1.0.tgz";
        url  = "https://registry.yarnpkg.com/safe-regex/-/safe-regex-1.1.0.tgz";
        sha1 = "40a3669f3b077d1e943d44629e157dd48023bf2e";
      };
    }

    {
      name = "safer-buffer-2.1.2.tgz";
      path = fetchurl {
        name = "safer-buffer-2.1.2.tgz";
        url  = "https://registry.yarnpkg.com/safer-buffer/-/safer-buffer-2.1.2.tgz";
        sha1 = "44fa161b0187b9549dd84bb91802f9bd8385cd6a";
      };
    }

    {
      name = "sass-graph-2.2.4.tgz";
      path = fetchurl {
        name = "sass-graph-2.2.4.tgz";
        url  = "https://registry.yarnpkg.com/sass-graph/-/sass-graph-2.2.4.tgz";
        sha1 = "13fbd63cd1caf0908b9fd93476ad43a51d1e0b49";
      };
    }

    {
      name = "sass-loader-7.3.1.tgz";
      path = fetchurl {
        name = "sass-loader-7.3.1.tgz";
        url  = "https://registry.yarnpkg.com/sass-loader/-/sass-loader-7.3.1.tgz";
        sha1 = "a5bf68a04bcea1c13ff842d747150f7ab7d0d23f";
      };
    }

    {
      name = "schema-utils-0.3.0.tgz";
      path = fetchurl {
        name = "schema-utils-0.3.0.tgz";
        url  = "https://registry.yarnpkg.com/schema-utils/-/schema-utils-0.3.0.tgz";
        sha1 = "f5877222ce3e931edae039f17eb3716e7137f8cf";
      };
    }

    {
      name = "schema-utils-1.0.0.tgz";
      path = fetchurl {
        name = "schema-utils-1.0.0.tgz";
        url  = "https://registry.yarnpkg.com/schema-utils/-/schema-utils-1.0.0.tgz";
        sha1 = "0b79a93204d7b600d4b2850d1f66c2a34951c770";
      };
    }

    {
      name = "scss-tokenizer-0.2.3.tgz";
      path = fetchurl {
        name = "scss-tokenizer-0.2.3.tgz";
        url  = "https://registry.yarnpkg.com/scss-tokenizer/-/scss-tokenizer-0.2.3.tgz";
        sha1 = "8eb06db9a9723333824d3f5530641149847ce5d1";
      };
    }

    {
      name = "select-hose-2.0.0.tgz";
      path = fetchurl {
        name = "select-hose-2.0.0.tgz";
        url  = "https://registry.yarnpkg.com/select-hose/-/select-hose-2.0.0.tgz";
        sha1 = "625d8658f865af43ec962bfc376a37359a4994ca";
      };
    }

    {
      name = "selfsigned-1.10.7.tgz";
      path = fetchurl {
        name = "selfsigned-1.10.7.tgz";
        url  = "https://registry.yarnpkg.com/selfsigned/-/selfsigned-1.10.7.tgz";
        sha1 = "da5819fd049d5574f28e88a9bcc6dbc6e6f3906b";
      };
    }

    {
      name = "semver-5.7.1.tgz";
      path = fetchurl {
        name = "semver-5.7.1.tgz";
        url  = "https://registry.yarnpkg.com/semver/-/semver-5.7.1.tgz";
        sha1 = "a954f931aeba508d307bbf069eff0c01c96116f7";
      };
    }

    {
      name = "semver-6.3.0.tgz";
      path = fetchurl {
        name = "semver-6.3.0.tgz";
        url  = "https://registry.yarnpkg.com/semver/-/semver-6.3.0.tgz";
        sha1 = "ee0a64c8af5e8ceea67687b133761e1becbd1d3d";
      };
    }

    {
      name = "semver-5.3.0.tgz";
      path = fetchurl {
        name = "semver-5.3.0.tgz";
        url  = "https://registry.yarnpkg.com/semver/-/semver-5.3.0.tgz";
        sha1 = "9b2ce5d3de02d17c6012ad326aa6b4d0cf54f94f";
      };
    }

    {
      name = "send-0.17.1.tgz";
      path = fetchurl {
        name = "send-0.17.1.tgz";
        url  = "https://registry.yarnpkg.com/send/-/send-0.17.1.tgz";
        sha1 = "c1d8b059f7900f7466dd4938bdc44e11ddb376c8";
      };
    }

    {
      name = "serialize-javascript-2.1.2.tgz";
      path = fetchurl {
        name = "serialize-javascript-2.1.2.tgz";
        url  = "https://registry.yarnpkg.com/serialize-javascript/-/serialize-javascript-2.1.2.tgz";
        sha1 = "ecec53b0e0317bdc95ef76ab7074b7384785fa61";
      };
    }

    {
      name = "serve-index-1.9.1.tgz";
      path = fetchurl {
        name = "serve-index-1.9.1.tgz";
        url  = "https://registry.yarnpkg.com/serve-index/-/serve-index-1.9.1.tgz";
        sha1 = "d3768d69b1e7d82e5ce050fff5b453bea12a9239";
      };
    }

    {
      name = "serve-static-1.14.1.tgz";
      path = fetchurl {
        name = "serve-static-1.14.1.tgz";
        url  = "https://registry.yarnpkg.com/serve-static/-/serve-static-1.14.1.tgz";
        sha1 = "666e636dc4f010f7ef29970a88a674320898b2f9";
      };
    }

    {
      name = "set-blocking-2.0.0.tgz";
      path = fetchurl {
        name = "set-blocking-2.0.0.tgz";
        url  = "https://registry.yarnpkg.com/set-blocking/-/set-blocking-2.0.0.tgz";
        sha1 = "045f9782d011ae9a6803ddd382b24392b3d890f7";
      };
    }

    {
      name = "set-value-2.0.1.tgz";
      path = fetchurl {
        name = "set-value-2.0.1.tgz";
        url  = "https://registry.yarnpkg.com/set-value/-/set-value-2.0.1.tgz";
        sha1 = "a18d40530e6f07de4228c7defe4227af8cad005b";
      };
    }

    {
      name = "setimmediate-1.0.5.tgz";
      path = fetchurl {
        name = "setimmediate-1.0.5.tgz";
        url  = "https://registry.yarnpkg.com/setimmediate/-/setimmediate-1.0.5.tgz";
        sha1 = "290cbb232e306942d7d7ea9b83732ab7856f8285";
      };
    }

    {
      name = "setprototypeof-1.1.0.tgz";
      path = fetchurl {
        name = "setprototypeof-1.1.0.tgz";
        url  = "https://registry.yarnpkg.com/setprototypeof/-/setprototypeof-1.1.0.tgz";
        sha1 = "d0bd85536887b6fe7c0d818cb962d9d91c54e656";
      };
    }

    {
      name = "setprototypeof-1.1.1.tgz";
      path = fetchurl {
        name = "setprototypeof-1.1.1.tgz";
        url  = "https://registry.yarnpkg.com/setprototypeof/-/setprototypeof-1.1.1.tgz";
        sha1 = "7e95acb24aa92f5885e0abef5ba131330d4ae683";
      };
    }

    {
      name = "sha.js-2.4.11.tgz";
      path = fetchurl {
        name = "sha.js-2.4.11.tgz";
        url  = "https://registry.yarnpkg.com/sha.js/-/sha.js-2.4.11.tgz";
        sha1 = "37a5cf0b81ecbc6943de109ba2960d1b26584ae7";
      };
    }

    {
      name = "shallow-clone-3.0.1.tgz";
      path = fetchurl {
        name = "shallow-clone-3.0.1.tgz";
        url  = "https://registry.yarnpkg.com/shallow-clone/-/shallow-clone-3.0.1.tgz";
        sha1 = "8f2981ad92531f55035b01fb230769a40e02efa3";
      };
    }

    {
      name = "shebang-command-1.2.0.tgz";
      path = fetchurl {
        name = "shebang-command-1.2.0.tgz";
        url  = "https://registry.yarnpkg.com/shebang-command/-/shebang-command-1.2.0.tgz";
        sha1 = "44aac65b695b03398968c39f363fee5deafdf1ea";
      };
    }

    {
      name = "shebang-regex-1.0.0.tgz";
      path = fetchurl {
        name = "shebang-regex-1.0.0.tgz";
        url  = "https://registry.yarnpkg.com/shebang-regex/-/shebang-regex-1.0.0.tgz";
        sha1 = "da42f49740c0b42db2ca9728571cb190c98efea3";
      };
    }

    {
      name = "signal-exit-3.0.2.tgz";
      path = fetchurl {
        name = "signal-exit-3.0.2.tgz";
        url  = "https://registry.yarnpkg.com/signal-exit/-/signal-exit-3.0.2.tgz";
        sha1 = "b5fdc08f1287ea1178628e415e25132b73646c6d";
      };
    }

    {
      name = "snapdragon-node-2.1.1.tgz";
      path = fetchurl {
        name = "snapdragon-node-2.1.1.tgz";
        url  = "https://registry.yarnpkg.com/snapdragon-node/-/snapdragon-node-2.1.1.tgz";
        sha1 = "6c175f86ff14bdb0724563e8f3c1b021a286853b";
      };
    }

    {
      name = "snapdragon-util-3.0.1.tgz";
      path = fetchurl {
        name = "snapdragon-util-3.0.1.tgz";
        url  = "https://registry.yarnpkg.com/snapdragon-util/-/snapdragon-util-3.0.1.tgz";
        sha1 = "f956479486f2acd79700693f6f7b805e45ab56e2";
      };
    }

    {
      name = "snapdragon-0.8.2.tgz";
      path = fetchurl {
        name = "snapdragon-0.8.2.tgz";
        url  = "https://registry.yarnpkg.com/snapdragon/-/snapdragon-0.8.2.tgz";
        sha1 = "64922e7c565b0e14204ba1aa7d6964278d25182d";
      };
    }

    {
      name = "sockjs-client-1.4.0.tgz";
      path = fetchurl {
        name = "sockjs-client-1.4.0.tgz";
        url  = "https://registry.yarnpkg.com/sockjs-client/-/sockjs-client-1.4.0.tgz";
        sha1 = "c9f2568e19c8fd8173b4997ea3420e0bb306c7d5";
      };
    }

    {
      name = "sockjs-0.3.19.tgz";
      path = fetchurl {
        name = "sockjs-0.3.19.tgz";
        url  = "https://registry.yarnpkg.com/sockjs/-/sockjs-0.3.19.tgz";
        sha1 = "d976bbe800af7bd20ae08598d582393508993c0d";
      };
    }

    {
      name = "source-list-map-2.0.1.tgz";
      path = fetchurl {
        name = "source-list-map-2.0.1.tgz";
        url  = "https://registry.yarnpkg.com/source-list-map/-/source-list-map-2.0.1.tgz";
        sha1 = "3993bd873bfc48479cca9ea3a547835c7c154b34";
      };
    }

    {
      name = "source-map-resolve-0.5.3.tgz";
      path = fetchurl {
        name = "source-map-resolve-0.5.3.tgz";
        url  = "https://registry.yarnpkg.com/source-map-resolve/-/source-map-resolve-0.5.3.tgz";
        sha1 = "190866bece7553e1f8f267a2ee82c606b5509a1a";
      };
    }

    {
      name = "source-map-support-0.5.16.tgz";
      path = fetchurl {
        name = "source-map-support-0.5.16.tgz";
        url  = "https://registry.yarnpkg.com/source-map-support/-/source-map-support-0.5.16.tgz";
        sha1 = "0ae069e7fe3ba7538c64c98515e35339eac5a042";
      };
    }

    {
      name = "source-map-url-0.4.0.tgz";
      path = fetchurl {
        name = "source-map-url-0.4.0.tgz";
        url  = "https://registry.yarnpkg.com/source-map-url/-/source-map-url-0.4.0.tgz";
        sha1 = "3e935d7ddd73631b97659956d55128e87b5084a3";
      };
    }

    {
      name = "source-map-0.4.4.tgz";
      path = fetchurl {
        name = "source-map-0.4.4.tgz";
        url  = "https://registry.yarnpkg.com/source-map/-/source-map-0.4.4.tgz";
        sha1 = "eba4f5da9c0dc999de68032d8b4f76173652036b";
      };
    }

    {
      name = "source-map-0.5.7.tgz";
      path = fetchurl {
        name = "source-map-0.5.7.tgz";
        url  = "https://registry.yarnpkg.com/source-map/-/source-map-0.5.7.tgz";
        sha1 = "8a039d2d1021d22d1ea14c80d8ea468ba2ef3fcc";
      };
    }

    {
      name = "source-map-0.6.1.tgz";
      path = fetchurl {
        name = "source-map-0.6.1.tgz";
        url  = "https://registry.yarnpkg.com/source-map/-/source-map-0.6.1.tgz";
        sha1 = "74722af32e9614e9c287a8d0bbde48b5e2f1a263";
      };
    }

    {
      name = "spdx-correct-3.1.0.tgz";
      path = fetchurl {
        name = "spdx-correct-3.1.0.tgz";
        url  = "https://registry.yarnpkg.com/spdx-correct/-/spdx-correct-3.1.0.tgz";
        sha1 = "fb83e504445268f154b074e218c87c003cd31df4";
      };
    }

    {
      name = "spdx-exceptions-2.2.0.tgz";
      path = fetchurl {
        name = "spdx-exceptions-2.2.0.tgz";
        url  = "https://registry.yarnpkg.com/spdx-exceptions/-/spdx-exceptions-2.2.0.tgz";
        sha1 = "2ea450aee74f2a89bfb94519c07fcd6f41322977";
      };
    }

    {
      name = "spdx-expression-parse-3.0.0.tgz";
      path = fetchurl {
        name = "spdx-expression-parse-3.0.0.tgz";
        url  = "https://registry.yarnpkg.com/spdx-expression-parse/-/spdx-expression-parse-3.0.0.tgz";
        sha1 = "99e119b7a5da00e05491c9fa338b7904823b41d0";
      };
    }

    {
      name = "spdx-license-ids-3.0.5.tgz";
      path = fetchurl {
        name = "spdx-license-ids-3.0.5.tgz";
        url  = "https://registry.yarnpkg.com/spdx-license-ids/-/spdx-license-ids-3.0.5.tgz";
        sha1 = "3694b5804567a458d3c8045842a6358632f62654";
      };
    }

    {
      name = "spdy-transport-3.0.0.tgz";
      path = fetchurl {
        name = "spdy-transport-3.0.0.tgz";
        url  = "https://registry.yarnpkg.com/spdy-transport/-/spdy-transport-3.0.0.tgz";
        sha1 = "00d4863a6400ad75df93361a1608605e5dcdcf31";
      };
    }

    {
      name = "spdy-4.0.1.tgz";
      path = fetchurl {
        name = "spdy-4.0.1.tgz";
        url  = "https://registry.yarnpkg.com/spdy/-/spdy-4.0.1.tgz";
        sha1 = "6f12ed1c5db7ea4f24ebb8b89ba58c87c08257f2";
      };
    }

    {
      name = "split-string-3.1.0.tgz";
      path = fetchurl {
        name = "split-string-3.1.0.tgz";
        url  = "https://registry.yarnpkg.com/split-string/-/split-string-3.1.0.tgz";
        sha1 = "7cb09dda3a86585705c64b39a6466038682e8fe2";
      };
    }

    {
      name = "sshpk-1.16.1.tgz";
      path = fetchurl {
        name = "sshpk-1.16.1.tgz";
        url  = "https://registry.yarnpkg.com/sshpk/-/sshpk-1.16.1.tgz";
        sha1 = "fb661c0bef29b39db40769ee39fa70093d6f6877";
      };
    }

    {
      name = "ssri-6.0.1.tgz";
      path = fetchurl {
        name = "ssri-6.0.1.tgz";
        url  = "https://registry.yarnpkg.com/ssri/-/ssri-6.0.1.tgz";
        sha1 = "2a3c41b28dd45b62b63676ecb74001265ae9edd8";
      };
    }

    {
      name = "static-extend-0.1.2.tgz";
      path = fetchurl {
        name = "static-extend-0.1.2.tgz";
        url  = "https://registry.yarnpkg.com/static-extend/-/static-extend-0.1.2.tgz";
        sha1 = "60809c39cbff55337226fd5e0b520f341f1fb5c6";
      };
    }

    {
      name = "statuses-1.5.0.tgz";
      path = fetchurl {
        name = "statuses-1.5.0.tgz";
        url  = "https://registry.yarnpkg.com/statuses/-/statuses-1.5.0.tgz";
        sha1 = "161c7dac177659fd9811f43771fa99381478628c";
      };
    }

    {
      name = "stdout-stream-1.4.1.tgz";
      path = fetchurl {
        name = "stdout-stream-1.4.1.tgz";
        url  = "https://registry.yarnpkg.com/stdout-stream/-/stdout-stream-1.4.1.tgz";
        sha1 = "5ac174cdd5cd726104aa0c0b2bd83815d8d535de";
      };
    }

    {
      name = "stream-browserify-2.0.2.tgz";
      path = fetchurl {
        name = "stream-browserify-2.0.2.tgz";
        url  = "https://registry.yarnpkg.com/stream-browserify/-/stream-browserify-2.0.2.tgz";
        sha1 = "87521d38a44aa7ee91ce1cd2a47df0cb49dd660b";
      };
    }

    {
      name = "stream-each-1.2.3.tgz";
      path = fetchurl {
        name = "stream-each-1.2.3.tgz";
        url  = "https://registry.yarnpkg.com/stream-each/-/stream-each-1.2.3.tgz";
        sha1 = "ebe27a0c389b04fbcc233642952e10731afa9bae";
      };
    }

    {
      name = "stream-http-2.8.3.tgz";
      path = fetchurl {
        name = "stream-http-2.8.3.tgz";
        url  = "https://registry.yarnpkg.com/stream-http/-/stream-http-2.8.3.tgz";
        sha1 = "b2d242469288a5a27ec4fe8933acf623de6514fc";
      };
    }

    {
      name = "stream-shift-1.0.1.tgz";
      path = fetchurl {
        name = "stream-shift-1.0.1.tgz";
        url  = "https://registry.yarnpkg.com/stream-shift/-/stream-shift-1.0.1.tgz";
        sha1 = "d7088281559ab2778424279b0877da3c392d5a3d";
      };
    }

    {
      name = "string-width-1.0.2.tgz";
      path = fetchurl {
        name = "string-width-1.0.2.tgz";
        url  = "https://registry.yarnpkg.com/string-width/-/string-width-1.0.2.tgz";
        sha1 = "118bdf5b8cdc51a2a7e70d211e07e2b0b9b107d3";
      };
    }

    {
      name = "string-width-2.1.1.tgz";
      path = fetchurl {
        name = "string-width-2.1.1.tgz";
        url  = "https://registry.yarnpkg.com/string-width/-/string-width-2.1.1.tgz";
        sha1 = "ab93f27a8dc13d28cac815c462143a6d9012ae9e";
      };
    }

    {
      name = "string-width-3.1.0.tgz";
      path = fetchurl {
        name = "string-width-3.1.0.tgz";
        url  = "https://registry.yarnpkg.com/string-width/-/string-width-3.1.0.tgz";
        sha1 = "22767be21b62af1081574306f69ac51b62203961";
      };
    }

    {
      name = "string.prototype.trimleft-2.1.1.tgz";
      path = fetchurl {
        name = "string.prototype.trimleft-2.1.1.tgz";
        url  = "https://registry.yarnpkg.com/string.prototype.trimleft/-/string.prototype.trimleft-2.1.1.tgz";
        sha1 = "9bdb8ac6abd6d602b17a4ed321870d2f8dcefc74";
      };
    }

    {
      name = "string.prototype.trimright-2.1.1.tgz";
      path = fetchurl {
        name = "string.prototype.trimright-2.1.1.tgz";
        url  = "https://registry.yarnpkg.com/string.prototype.trimright/-/string.prototype.trimright-2.1.1.tgz";
        sha1 = "440314b15996c866ce8a0341894d45186200c5d9";
      };
    }

    {
      name = "string_decoder-1.3.0.tgz";
      path = fetchurl {
        name = "string_decoder-1.3.0.tgz";
        url  = "https://registry.yarnpkg.com/string_decoder/-/string_decoder-1.3.0.tgz";
        sha1 = "42f114594a46cf1a8e30b0a84f56c78c3edac21e";
      };
    }

    {
      name = "string_decoder-1.1.1.tgz";
      path = fetchurl {
        name = "string_decoder-1.1.1.tgz";
        url  = "https://registry.yarnpkg.com/string_decoder/-/string_decoder-1.1.1.tgz";
        sha1 = "9cf1611ba62685d7030ae9e4ba34149c3af03fc8";
      };
    }

    {
      name = "strip-ansi-3.0.1.tgz";
      path = fetchurl {
        name = "strip-ansi-3.0.1.tgz";
        url  = "https://registry.yarnpkg.com/strip-ansi/-/strip-ansi-3.0.1.tgz";
        sha1 = "6a385fb8853d952d5ff05d0e8aaf94278dc63dcf";
      };
    }

    {
      name = "strip-ansi-4.0.0.tgz";
      path = fetchurl {
        name = "strip-ansi-4.0.0.tgz";
        url  = "https://registry.yarnpkg.com/strip-ansi/-/strip-ansi-4.0.0.tgz";
        sha1 = "a8479022eb1ac368a871389b635262c505ee368f";
      };
    }

    {
      name = "strip-ansi-5.2.0.tgz";
      path = fetchurl {
        name = "strip-ansi-5.2.0.tgz";
        url  = "https://registry.yarnpkg.com/strip-ansi/-/strip-ansi-5.2.0.tgz";
        sha1 = "8c9a536feb6afc962bdfa5b104a5091c1ad9c0ae";
      };
    }

    {
      name = "strip-bom-2.0.0.tgz";
      path = fetchurl {
        name = "strip-bom-2.0.0.tgz";
        url  = "https://registry.yarnpkg.com/strip-bom/-/strip-bom-2.0.0.tgz";
        sha1 = "6219a85616520491f35788bdbf1447a99c7e6b0e";
      };
    }

    {
      name = "strip-eof-1.0.0.tgz";
      path = fetchurl {
        name = "strip-eof-1.0.0.tgz";
        url  = "https://registry.yarnpkg.com/strip-eof/-/strip-eof-1.0.0.tgz";
        sha1 = "bb43ff5598a6eb05d89b59fcd129c983313606bf";
      };
    }

    {
      name = "strip-indent-1.0.1.tgz";
      path = fetchurl {
        name = "strip-indent-1.0.1.tgz";
        url  = "https://registry.yarnpkg.com/strip-indent/-/strip-indent-1.0.1.tgz";
        sha1 = "0c7962a6adefa7bbd4ac366460a638552ae1a0a2";
      };
    }

    {
      name = "style-loader-0.23.1.tgz";
      path = fetchurl {
        name = "style-loader-0.23.1.tgz";
        url  = "https://registry.yarnpkg.com/style-loader/-/style-loader-0.23.1.tgz";
        sha1 = "cb9154606f3e771ab6c4ab637026a1049174d925";
      };
    }

    {
      name = "supports-color-6.1.0.tgz";
      path = fetchurl {
        name = "supports-color-6.1.0.tgz";
        url  = "https://registry.yarnpkg.com/supports-color/-/supports-color-6.1.0.tgz";
        sha1 = "0764abc69c63d5ac842dd4867e8d025e880df8f3";
      };
    }

    {
      name = "supports-color-2.0.0.tgz";
      path = fetchurl {
        name = "supports-color-2.0.0.tgz";
        url  = "https://registry.yarnpkg.com/supports-color/-/supports-color-2.0.0.tgz";
        sha1 = "535d045ce6b6363fa40117084629995e9df324c7";
      };
    }

    {
      name = "supports-color-5.5.0.tgz";
      path = fetchurl {
        name = "supports-color-5.5.0.tgz";
        url  = "https://registry.yarnpkg.com/supports-color/-/supports-color-5.5.0.tgz";
        sha1 = "e2e69a44ac8772f78a1ec0b35b689df6530efc8f";
      };
    }

    {
      name = "tapable-1.1.3.tgz";
      path = fetchurl {
        name = "tapable-1.1.3.tgz";
        url  = "https://registry.yarnpkg.com/tapable/-/tapable-1.1.3.tgz";
        sha1 = "a1fccc06b58db61fd7a45da2da44f5f3a3e67ba2";
      };
    }

    {
      name = "tar-2.2.2.tgz";
      path = fetchurl {
        name = "tar-2.2.2.tgz";
        url  = "https://registry.yarnpkg.com/tar/-/tar-2.2.2.tgz";
        sha1 = "0ca8848562c7299b8b446ff6a4d60cdbb23edc40";
      };
    }

    {
      name = "terser-webpack-plugin-1.4.3.tgz";
      path = fetchurl {
        name = "terser-webpack-plugin-1.4.3.tgz";
        url  = "https://registry.yarnpkg.com/terser-webpack-plugin/-/terser-webpack-plugin-1.4.3.tgz";
        sha1 = "5ecaf2dbdc5fb99745fd06791f46fc9ddb1c9a7c";
      };
    }

    {
      name = "terser-4.6.3.tgz";
      path = fetchurl {
        name = "terser-4.6.3.tgz";
        url  = "https://registry.yarnpkg.com/terser/-/terser-4.6.3.tgz";
        sha1 = "e33aa42461ced5238d352d2df2a67f21921f8d87";
      };
    }

    {
      name = "through2-2.0.5.tgz";
      path = fetchurl {
        name = "through2-2.0.5.tgz";
        url  = "https://registry.yarnpkg.com/through2/-/through2-2.0.5.tgz";
        sha1 = "01c1e39eb31d07cb7d03a96a70823260b23132cd";
      };
    }

    {
      name = "thunky-1.1.0.tgz";
      path = fetchurl {
        name = "thunky-1.1.0.tgz";
        url  = "https://registry.yarnpkg.com/thunky/-/thunky-1.1.0.tgz";
        sha1 = "5abaf714a9405db0504732bbccd2cedd9ef9537d";
      };
    }

    {
      name = "timers-browserify-2.0.11.tgz";
      path = fetchurl {
        name = "timers-browserify-2.0.11.tgz";
        url  = "https://registry.yarnpkg.com/timers-browserify/-/timers-browserify-2.0.11.tgz";
        sha1 = "800b1f3eee272e5bc53ee465a04d0e804c31211f";
      };
    }

    {
      name = "to-arraybuffer-1.0.1.tgz";
      path = fetchurl {
        name = "to-arraybuffer-1.0.1.tgz";
        url  = "https://registry.yarnpkg.com/to-arraybuffer/-/to-arraybuffer-1.0.1.tgz";
        sha1 = "7d229b1fcc637e466ca081180836a7aabff83f43";
      };
    }

    {
      name = "to-object-path-0.3.0.tgz";
      path = fetchurl {
        name = "to-object-path-0.3.0.tgz";
        url  = "https://registry.yarnpkg.com/to-object-path/-/to-object-path-0.3.0.tgz";
        sha1 = "297588b7b0e7e0ac08e04e672f85c1f4999e17af";
      };
    }

    {
      name = "to-regex-range-2.1.1.tgz";
      path = fetchurl {
        name = "to-regex-range-2.1.1.tgz";
        url  = "https://registry.yarnpkg.com/to-regex-range/-/to-regex-range-2.1.1.tgz";
        sha1 = "7c80c17b9dfebe599e27367e0d4dd5590141db38";
      };
    }

    {
      name = "to-regex-3.0.2.tgz";
      path = fetchurl {
        name = "to-regex-3.0.2.tgz";
        url  = "https://registry.yarnpkg.com/to-regex/-/to-regex-3.0.2.tgz";
        sha1 = "13cfdd9b336552f30b51f33a8ae1b42a7a7599ce";
      };
    }

    {
      name = "toidentifier-1.0.0.tgz";
      path = fetchurl {
        name = "toidentifier-1.0.0.tgz";
        url  = "https://registry.yarnpkg.com/toidentifier/-/toidentifier-1.0.0.tgz";
        sha1 = "7e1be3470f1e77948bc43d94a3c8f4d7752ba553";
      };
    }

    {
      name = "toposort-1.0.7.tgz";
      path = fetchurl {
        name = "toposort-1.0.7.tgz";
        url  = "https://registry.yarnpkg.com/toposort/-/toposort-1.0.7.tgz";
        sha1 = "2e68442d9f64ec720b8cc89e6443ac6caa950029";
      };
    }

    {
      name = "tough-cookie-2.5.0.tgz";
      path = fetchurl {
        name = "tough-cookie-2.5.0.tgz";
        url  = "https://registry.yarnpkg.com/tough-cookie/-/tough-cookie-2.5.0.tgz";
        sha1 = "cd9fb2a0aa1d5a12b473bd9fb96fa3dcff65ade2";
      };
    }

    {
      name = "trim-newlines-1.0.0.tgz";
      path = fetchurl {
        name = "trim-newlines-1.0.0.tgz";
        url  = "https://registry.yarnpkg.com/trim-newlines/-/trim-newlines-1.0.0.tgz";
        sha1 = "5887966bb582a4503a41eb524f7d35011815a613";
      };
    }

    {
      name = "true-case-path-1.0.3.tgz";
      path = fetchurl {
        name = "true-case-path-1.0.3.tgz";
        url  = "https://registry.yarnpkg.com/true-case-path/-/true-case-path-1.0.3.tgz";
        sha1 = "f813b5a8c86b40da59606722b144e3225799f47d";
      };
    }

    {
      name = "tslib-1.10.0.tgz";
      path = fetchurl {
        name = "tslib-1.10.0.tgz";
        url  = "https://registry.yarnpkg.com/tslib/-/tslib-1.10.0.tgz";
        sha1 = "c3c19f95973fb0a62973fb09d90d961ee43e5c8a";
      };
    }

    {
      name = "tty-browserify-0.0.0.tgz";
      path = fetchurl {
        name = "tty-browserify-0.0.0.tgz";
        url  = "https://registry.yarnpkg.com/tty-browserify/-/tty-browserify-0.0.0.tgz";
        sha1 = "a157ba402da24e9bf957f9aa69d524eed42901a6";
      };
    }

    {
      name = "tunnel-agent-0.6.0.tgz";
      path = fetchurl {
        name = "tunnel-agent-0.6.0.tgz";
        url  = "https://registry.yarnpkg.com/tunnel-agent/-/tunnel-agent-0.6.0.tgz";
        sha1 = "27a5dea06b36b04a0a9966774b290868f0fc40fd";
      };
    }

    {
      name = "tweetnacl-0.14.5.tgz";
      path = fetchurl {
        name = "tweetnacl-0.14.5.tgz";
        url  = "https://registry.yarnpkg.com/tweetnacl/-/tweetnacl-0.14.5.tgz";
        sha1 = "5ae68177f192d4456269d108afa93ff8743f4f64";
      };
    }

    {
      name = "type-is-1.6.18.tgz";
      path = fetchurl {
        name = "type-is-1.6.18.tgz";
        url  = "https://registry.yarnpkg.com/type-is/-/type-is-1.6.18.tgz";
        sha1 = "4e552cd05df09467dcbc4ef739de89f2cf37c131";
      };
    }

    {
      name = "typedarray-0.0.6.tgz";
      path = fetchurl {
        name = "typedarray-0.0.6.tgz";
        url  = "https://registry.yarnpkg.com/typedarray/-/typedarray-0.0.6.tgz";
        sha1 = "867ac74e3864187b1d3d47d996a78ec5c8830777";
      };
    }

    {
      name = "uglify-js-3.4.10.tgz";
      path = fetchurl {
        name = "uglify-js-3.4.10.tgz";
        url  = "https://registry.yarnpkg.com/uglify-js/-/uglify-js-3.4.10.tgz";
        sha1 = "9ad9563d8eb3acdfb8d38597d2af1d815f6a755f";
      };
    }

    {
      name = "union-value-1.0.1.tgz";
      path = fetchurl {
        name = "union-value-1.0.1.tgz";
        url  = "https://registry.yarnpkg.com/union-value/-/union-value-1.0.1.tgz";
        sha1 = "0b6fe7b835aecda61c6ea4d4f02c14221e109847";
      };
    }

    {
      name = "unique-filename-1.1.1.tgz";
      path = fetchurl {
        name = "unique-filename-1.1.1.tgz";
        url  = "https://registry.yarnpkg.com/unique-filename/-/unique-filename-1.1.1.tgz";
        sha1 = "1d69769369ada0583103a1e6ae87681b56573230";
      };
    }

    {
      name = "unique-slug-2.0.2.tgz";
      path = fetchurl {
        name = "unique-slug-2.0.2.tgz";
        url  = "https://registry.yarnpkg.com/unique-slug/-/unique-slug-2.0.2.tgz";
        sha1 = "baabce91083fc64e945b0f3ad613e264f7cd4e6c";
      };
    }

    {
      name = "unpipe-1.0.0.tgz";
      path = fetchurl {
        name = "unpipe-1.0.0.tgz";
        url  = "https://registry.yarnpkg.com/unpipe/-/unpipe-1.0.0.tgz";
        sha1 = "b2bf4ee8514aae6165b4817829d21b2ef49904ec";
      };
    }

    {
      name = "unset-value-1.0.0.tgz";
      path = fetchurl {
        name = "unset-value-1.0.0.tgz";
        url  = "https://registry.yarnpkg.com/unset-value/-/unset-value-1.0.0.tgz";
        sha1 = "8376873f7d2335179ffb1e6fc3a8ed0dfc8ab559";
      };
    }

    {
      name = "upath-1.2.0.tgz";
      path = fetchurl {
        name = "upath-1.2.0.tgz";
        url  = "https://registry.yarnpkg.com/upath/-/upath-1.2.0.tgz";
        sha1 = "8f66dbcd55a883acdae4408af8b035a5044c1894";
      };
    }

    {
      name = "upper-case-1.1.3.tgz";
      path = fetchurl {
        name = "upper-case-1.1.3.tgz";
        url  = "https://registry.yarnpkg.com/upper-case/-/upper-case-1.1.3.tgz";
        sha1 = "f6b4501c2ec4cdd26ba78be7222961de77621598";
      };
    }

    {
      name = "uri-js-4.2.2.tgz";
      path = fetchurl {
        name = "uri-js-4.2.2.tgz";
        url  = "https://registry.yarnpkg.com/uri-js/-/uri-js-4.2.2.tgz";
        sha1 = "94c540e1ff772956e2299507c010aea6c8838eb0";
      };
    }

    {
      name = "urix-0.1.0.tgz";
      path = fetchurl {
        name = "urix-0.1.0.tgz";
        url  = "https://registry.yarnpkg.com/urix/-/urix-0.1.0.tgz";
        sha1 = "da937f7a62e21fec1fd18d49b35c2935067a6c72";
      };
    }

    {
      name = "url-loader-1.1.2.tgz";
      path = fetchurl {
        name = "url-loader-1.1.2.tgz";
        url  = "https://registry.yarnpkg.com/url-loader/-/url-loader-1.1.2.tgz";
        sha1 = "b971d191b83af693c5e3fea4064be9e1f2d7f8d8";
      };
    }

    {
      name = "url-parse-1.4.7.tgz";
      path = fetchurl {
        name = "url-parse-1.4.7.tgz";
        url  = "https://registry.yarnpkg.com/url-parse/-/url-parse-1.4.7.tgz";
        sha1 = "a8a83535e8c00a316e403a5db4ac1b9b853ae278";
      };
    }

    {
      name = "url-0.11.0.tgz";
      path = fetchurl {
        name = "url-0.11.0.tgz";
        url  = "https://registry.yarnpkg.com/url/-/url-0.11.0.tgz";
        sha1 = "3838e97cfc60521eb73c525a8e55bfdd9e2e28f1";
      };
    }

    {
      name = "use-3.1.1.tgz";
      path = fetchurl {
        name = "use-3.1.1.tgz";
        url  = "https://registry.yarnpkg.com/use/-/use-3.1.1.tgz";
        sha1 = "d50c8cac79a19fbc20f2911f56eb973f4e10070f";
      };
    }

    {
      name = "util-deprecate-1.0.2.tgz";
      path = fetchurl {
        name = "util-deprecate-1.0.2.tgz";
        url  = "https://registry.yarnpkg.com/util-deprecate/-/util-deprecate-1.0.2.tgz";
        sha1 = "450d4dc9fa70de732762fbd2d4a28981419a0ccf";
      };
    }

    {
      name = "util.promisify-1.0.0.tgz";
      path = fetchurl {
        name = "util.promisify-1.0.0.tgz";
        url  = "https://registry.yarnpkg.com/util.promisify/-/util.promisify-1.0.0.tgz";
        sha1 = "440f7165a459c9a16dc145eb8e72f35687097030";
      };
    }

    {
      name = "util-0.10.3.tgz";
      path = fetchurl {
        name = "util-0.10.3.tgz";
        url  = "https://registry.yarnpkg.com/util/-/util-0.10.3.tgz";
        sha1 = "7afb1afe50805246489e3db7fe0ed379336ac0f9";
      };
    }

    {
      name = "util-0.11.1.tgz";
      path = fetchurl {
        name = "util-0.11.1.tgz";
        url  = "https://registry.yarnpkg.com/util/-/util-0.11.1.tgz";
        sha1 = "3236733720ec64bb27f6e26f421aaa2e1b588d61";
      };
    }

    {
      name = "utila-0.4.0.tgz";
      path = fetchurl {
        name = "utila-0.4.0.tgz";
        url  = "https://registry.yarnpkg.com/utila/-/utila-0.4.0.tgz";
        sha1 = "8a16a05d445657a3aea5eecc5b12a4fa5379772c";
      };
    }

    {
      name = "utils-merge-1.0.1.tgz";
      path = fetchurl {
        name = "utils-merge-1.0.1.tgz";
        url  = "https://registry.yarnpkg.com/utils-merge/-/utils-merge-1.0.1.tgz";
        sha1 = "9f95710f50a267947b2ccc124741c1028427e713";
      };
    }

    {
      name = "uuid-validate-0.0.3.tgz";
      path = fetchurl {
        name = "uuid-validate-0.0.3.tgz";
        url  = "https://registry.yarnpkg.com/uuid-validate/-/uuid-validate-0.0.3.tgz";
        sha1 = "e30617f75dc742a0e4f95012a11540faf9d39ab4";
      };
    }

    {
      name = "uuid-3.4.0.tgz";
      path = fetchurl {
        name = "uuid-3.4.0.tgz";
        url  = "https://registry.yarnpkg.com/uuid/-/uuid-3.4.0.tgz";
        sha1 = "b23e4358afa8a202fe7a100af1f5f883f02007ee";
      };
    }

    {
      name = "v8-compile-cache-2.0.3.tgz";
      path = fetchurl {
        name = "v8-compile-cache-2.0.3.tgz";
        url  = "https://registry.yarnpkg.com/v8-compile-cache/-/v8-compile-cache-2.0.3.tgz";
        sha1 = "00f7494d2ae2b688cfe2899df6ed2c54bef91dbe";
      };
    }

    {
      name = "validate-npm-package-license-3.0.4.tgz";
      path = fetchurl {
        name = "validate-npm-package-license-3.0.4.tgz";
        url  = "https://registry.yarnpkg.com/validate-npm-package-license/-/validate-npm-package-license-3.0.4.tgz";
        sha1 = "fc91f6b9c7ba15c857f4cb2c5defeec39d4f410a";
      };
    }

    {
      name = "vary-1.1.2.tgz";
      path = fetchurl {
        name = "vary-1.1.2.tgz";
        url  = "https://registry.yarnpkg.com/vary/-/vary-1.1.2.tgz";
        sha1 = "2299f02c6ded30d4a5961b0b9f74524a18f634fc";
      };
    }

    {
      name = "verror-1.10.0.tgz";
      path = fetchurl {
        name = "verror-1.10.0.tgz";
        url  = "https://registry.yarnpkg.com/verror/-/verror-1.10.0.tgz";
        sha1 = "3a105ca17053af55d6e270c1f8288682e18da400";
      };
    }

    {
      name = "vm-browserify-1.1.2.tgz";
      path = fetchurl {
        name = "vm-browserify-1.1.2.tgz";
        url  = "https://registry.yarnpkg.com/vm-browserify/-/vm-browserify-1.1.2.tgz";
        sha1 = "78641c488b8e6ca91a75f511e7a3b32a86e5dda0";
      };
    }

    {
      name = "watchpack-1.6.0.tgz";
      path = fetchurl {
        name = "watchpack-1.6.0.tgz";
        url  = "https://registry.yarnpkg.com/watchpack/-/watchpack-1.6.0.tgz";
        sha1 = "4bc12c2ebe8aa277a71f1d3f14d685c7b446cd00";
      };
    }

    {
      name = "wbuf-1.7.3.tgz";
      path = fetchurl {
        name = "wbuf-1.7.3.tgz";
        url  = "https://registry.yarnpkg.com/wbuf/-/wbuf-1.7.3.tgz";
        sha1 = "c1d8d149316d3ea852848895cb6a0bfe887b87df";
      };
    }

    {
      name = "webpack-cli-3.3.11.tgz";
      path = fetchurl {
        name = "webpack-cli-3.3.11.tgz";
        url  = "https://registry.yarnpkg.com/webpack-cli/-/webpack-cli-3.3.11.tgz";
        sha1 = "3bf21889bf597b5d82c38f215135a411edfdc631";
      };
    }

    {
      name = "webpack-dev-middleware-3.7.2.tgz";
      path = fetchurl {
        name = "webpack-dev-middleware-3.7.2.tgz";
        url  = "https://registry.yarnpkg.com/webpack-dev-middleware/-/webpack-dev-middleware-3.7.2.tgz";
        sha1 = "0019c3db716e3fa5cecbf64f2ab88a74bab331f3";
      };
    }

    {
      name = "webpack-dev-server-3.10.3.tgz";
      path = fetchurl {
        name = "webpack-dev-server-3.10.3.tgz";
        url  = "https://registry.yarnpkg.com/webpack-dev-server/-/webpack-dev-server-3.10.3.tgz";
        sha1 = "f35945036813e57ef582c2420ef7b470e14d3af0";
      };
    }

    {
      name = "webpack-log-2.0.0.tgz";
      path = fetchurl {
        name = "webpack-log-2.0.0.tgz";
        url  = "https://registry.yarnpkg.com/webpack-log/-/webpack-log-2.0.0.tgz";
        sha1 = "5b7928e0637593f119d32f6227c1e0ac31e1b47f";
      };
    }

    {
      name = "webpack-sources-1.4.3.tgz";
      path = fetchurl {
        name = "webpack-sources-1.4.3.tgz";
        url  = "https://registry.yarnpkg.com/webpack-sources/-/webpack-sources-1.4.3.tgz";
        sha1 = "eedd8ec0b928fbf1cbfe994e22d2d890f330a933";
      };
    }

    {
      name = "webpack-4.41.6.tgz";
      path = fetchurl {
        name = "webpack-4.41.6.tgz";
        url  = "https://registry.yarnpkg.com/webpack/-/webpack-4.41.6.tgz";
        sha1 = "12f2f804bf6542ef166755050d4afbc8f66ba7e1";
      };
    }

    {
      name = "websocket-driver-0.7.3.tgz";
      path = fetchurl {
        name = "websocket-driver-0.7.3.tgz";
        url  = "https://registry.yarnpkg.com/websocket-driver/-/websocket-driver-0.7.3.tgz";
        sha1 = "a2d4e0d4f4f116f1e6297eba58b05d430100e9f9";
      };
    }

    {
      name = "websocket-extensions-0.1.3.tgz";
      path = fetchurl {
        name = "websocket-extensions-0.1.3.tgz";
        url  = "https://registry.yarnpkg.com/websocket-extensions/-/websocket-extensions-0.1.3.tgz";
        sha1 = "5d2ff22977003ec687a4b87073dfbbac146ccf29";
      };
    }

    {
      name = "which-module-1.0.0.tgz";
      path = fetchurl {
        name = "which-module-1.0.0.tgz";
        url  = "https://registry.yarnpkg.com/which-module/-/which-module-1.0.0.tgz";
        sha1 = "bba63ca861948994ff307736089e3b96026c2a4f";
      };
    }

    {
      name = "which-module-2.0.0.tgz";
      path = fetchurl {
        name = "which-module-2.0.0.tgz";
        url  = "https://registry.yarnpkg.com/which-module/-/which-module-2.0.0.tgz";
        sha1 = "d9ef07dce77b9902b8a3a8fa4b31c3e3f7e6e87a";
      };
    }

    {
      name = "which-1.3.1.tgz";
      path = fetchurl {
        name = "which-1.3.1.tgz";
        url  = "https://registry.yarnpkg.com/which/-/which-1.3.1.tgz";
        sha1 = "a45043d54f5805316da8d62f9f50918d3da70b0a";
      };
    }

    {
      name = "wide-align-1.1.3.tgz";
      path = fetchurl {
        name = "wide-align-1.1.3.tgz";
        url  = "https://registry.yarnpkg.com/wide-align/-/wide-align-1.1.3.tgz";
        sha1 = "ae074e6bdc0c14a431e804e624549c633b000457";
      };
    }

    {
      name = "worker-farm-1.7.0.tgz";
      path = fetchurl {
        name = "worker-farm-1.7.0.tgz";
        url  = "https://registry.yarnpkg.com/worker-farm/-/worker-farm-1.7.0.tgz";
        sha1 = "26a94c5391bbca926152002f69b84a4bf772e5a8";
      };
    }

    {
      name = "wrap-ansi-2.1.0.tgz";
      path = fetchurl {
        name = "wrap-ansi-2.1.0.tgz";
        url  = "https://registry.yarnpkg.com/wrap-ansi/-/wrap-ansi-2.1.0.tgz";
        sha1 = "d8fc3d284dd05794fe84973caecdd1cf824fdd85";
      };
    }

    {
      name = "wrap-ansi-5.1.0.tgz";
      path = fetchurl {
        name = "wrap-ansi-5.1.0.tgz";
        url  = "https://registry.yarnpkg.com/wrap-ansi/-/wrap-ansi-5.1.0.tgz";
        sha1 = "1fd1f67235d5b6d0fee781056001bfb694c03b09";
      };
    }

    {
      name = "wrappy-1.0.2.tgz";
      path = fetchurl {
        name = "wrappy-1.0.2.tgz";
        url  = "https://registry.yarnpkg.com/wrappy/-/wrappy-1.0.2.tgz";
        sha1 = "b5243d8f3ec1aa35f1364605bc0d1036e30ab69f";
      };
    }

    {
      name = "ws-6.2.1.tgz";
      path = fetchurl {
        name = "ws-6.2.1.tgz";
        url  = "https://registry.yarnpkg.com/ws/-/ws-6.2.1.tgz";
        sha1 = "442fdf0a47ed64f59b6a5d8ff130f4748ed524fb";
      };
    }

    {
      name = "xhr2-0.1.4.tgz";
      path = fetchurl {
        name = "xhr2-0.1.4.tgz";
        url  = "https://registry.yarnpkg.com/xhr2/-/xhr2-0.1.4.tgz";
        sha1 = "7f87658847716db5026323812f818cadab387a5f";
      };
    }

    {
      name = "xtend-4.0.2.tgz";
      path = fetchurl {
        name = "xtend-4.0.2.tgz";
        url  = "https://registry.yarnpkg.com/xtend/-/xtend-4.0.2.tgz";
        sha1 = "bb72779f5fa465186b1f438f674fa347fdb5db54";
      };
    }

    {
      name = "y18n-3.2.1.tgz";
      path = fetchurl {
        name = "y18n-3.2.1.tgz";
        url  = "https://registry.yarnpkg.com/y18n/-/y18n-3.2.1.tgz";
        sha1 = "6d15fba884c08679c0d77e88e7759e811e07fa41";
      };
    }

    {
      name = "y18n-4.0.0.tgz";
      path = fetchurl {
        name = "y18n-4.0.0.tgz";
        url  = "https://registry.yarnpkg.com/y18n/-/y18n-4.0.0.tgz";
        sha1 = "95ef94f85ecc81d007c264e190a120f0a3c8566b";
      };
    }

    {
      name = "yallist-2.1.2.tgz";
      path = fetchurl {
        name = "yallist-2.1.2.tgz";
        url  = "https://registry.yarnpkg.com/yallist/-/yallist-2.1.2.tgz";
        sha1 = "1c11f9218f076089a47dd512f93c6699a6a81d52";
      };
    }

    {
      name = "yallist-3.1.1.tgz";
      path = fetchurl {
        name = "yallist-3.1.1.tgz";
        url  = "https://registry.yarnpkg.com/yallist/-/yallist-3.1.1.tgz";
        sha1 = "dbb7daf9bfd8bac9ab45ebf602b8cbad0d5d08fd";
      };
    }

    {
      name = "yargs-parser-11.1.1.tgz";
      path = fetchurl {
        name = "yargs-parser-11.1.1.tgz";
        url  = "https://registry.yarnpkg.com/yargs-parser/-/yargs-parser-11.1.1.tgz";
        sha1 = "879a0865973bca9f6bab5cbdf3b1c67ec7d3bcf4";
      };
    }

    {
      name = "yargs-parser-13.1.1.tgz";
      path = fetchurl {
        name = "yargs-parser-13.1.1.tgz";
        url  = "https://registry.yarnpkg.com/yargs-parser/-/yargs-parser-13.1.1.tgz";
        sha1 = "d26058532aa06d365fe091f6a1fc06b2f7e5eca0";
      };
    }

    {
      name = "yargs-parser-5.0.0.tgz";
      path = fetchurl {
        name = "yargs-parser-5.0.0.tgz";
        url  = "https://registry.yarnpkg.com/yargs-parser/-/yargs-parser-5.0.0.tgz";
        sha1 = "275ecf0d7ffe05c77e64e7c86e4cd94bf0e1228a";
      };
    }

    {
      name = "yargs-12.0.5.tgz";
      path = fetchurl {
        name = "yargs-12.0.5.tgz";
        url  = "https://registry.yarnpkg.com/yargs/-/yargs-12.0.5.tgz";
        sha1 = "05f5997b609647b64f66b81e3b4b10a368e7ad13";
      };
    }

    {
      name = "yargs-13.2.4.tgz";
      path = fetchurl {
        name = "yargs-13.2.4.tgz";
        url  = "https://registry.yarnpkg.com/yargs/-/yargs-13.2.4.tgz";
        sha1 = "0b562b794016eb9651b98bd37acf364aa5d6dc83";
      };
    }

    {
      name = "yargs-7.1.0.tgz";
      path = fetchurl {
        name = "yargs-7.1.0.tgz";
        url  = "https://registry.yarnpkg.com/yargs/-/yargs-7.1.0.tgz";
        sha1 = "6ba318eb16961727f5d284f8ea003e8d6154d0c8";
      };
    }

    {
      name = "zrender-4.2.0.tgz";
      path = fetchurl {
        name = "zrender-4.2.0.tgz";
        url  = "https://registry.yarnpkg.com/zrender/-/zrender-4.2.0.tgz";
        sha1 = "d001302e155f28de1f9fc7fcd5c254bad28471cf";
      };
    }
  ];
}
