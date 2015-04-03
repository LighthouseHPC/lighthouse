/*
 * Fortran brush for SyntaxHighlighter
 *
 * SyntaxHighlighter by Alex Gorbatchev
 * http://alexgorbatchev.com/
 *
 * Fortran brush by Shawn Chin
 * Last update : 16 April 2011
 * Homepage    : http://shawnchin.github.com
 * Brush page  : http://gist.github.com/gists/517349
 *
 */
;(function()
{
  //CommonJS
  typeof(require) != 'undefined' ? SyntaxHighlighter = require('shCore').SyntaxHighlighter : null;

  function Brush() {

    // Fortran 77 keywords
    var keywords = "access assign backspace blank block call close common ";
    keywords += "continue data dimension direct do else endif enddo ";
    keywords += "end entry eof equivalence err exist external file ";
    keywords += "fmt form format formatted function goto if implicit ";
    keywords += "include inquire intrinsic iostat logical named ";
    keywords += "namelist nextrec number open opened parameter pause ";
    keywords += "print program read rec recl return rewind sequential ";
    keywords += "status stop subroutine then type unformatted unit write save";

    // Fortran 90 keywords
    keywords += "allocate allocatable case contains cycle deallocate default ";
    keywords += "elsewhere exit interface intent module only operator ";
    keywords += "optional pointer private procedure public result recursive ";
    keywords += "select sequence target use while where ";

    // Fortran 95 keywords
    keywords += "elemental forall pure ";

    // Fortran 2003 keywords
    keywords += "abstract associate class decimal decorate delegate encoding ";
    keywords += "endfile enum enumerator extends extensible flush generic ";
    keywords += "iomsg import move_alloc nextrec non_overridable pass ";
    keywords += "pending reference round sign static typealias ";

    // Fortran 2003 attributes
    keywords += "asynchronous bind protected volatile ";

    // some non-standard keywords accepted by specific compilers
    // keywords += "accept array byte decode encode extrinsic nullify none options ";

    // standard datatypes
    var datatypes = "character complex double precision double ";
    datatypes += "complex integer logical real ";

    // Fortran 77 intrinsic functions
    var functions = "abs achar acos aimag aint alog alog10 amax0 amax1 ";
    functions += "amin0 amin1 amod anint asin atan atan2 cabs ccos cexp ";
    functions += "char clog cmplx conjg cos cosh csin csqrt dabs dacos ";
    functions += "dasin datan datan2 dble dcos dcosh ddim dexp dim dint ";
    functions += "dlog dlog10 dmax1 dmin1 dmod dnint dprod dsign dsinh dsin ";
    functions += "dsqrt dtanh dtan dtime exp float iabs idim idint idnint ";
    functions += "ifix index int isign len lge lgt lle llt log log10 max ";
    functions += "min mod nint real sign sin sngl sqrt tan tanh ";

    // Fortran 95 intrinsic functions
    functions += "adjustl adjustr all allocated any associated bit_size ";
    functions += "btest ceiling count cpu_time cshift date_and_time digits ";
    functions += "dot_product eoshift epsilon exponent floor fraction ";
    functions += "huge iachar iand ibclr ibits ibset ichar ieor ior ";
    functions += "ishft ishftc kind lbound len_trim logical matmul ";
    functions += "maxexponent maxloc maxval merge minexponent minloc ";
    functions += "minval modulo mvbits nearest not null pack precision ";
    functions += "present product radix random_number random_seed range ";
    functions += "repeat reshape rrspacing scale scan selected_int_kind ";
    functions += "selected_real_kind set_exponent shape sinh size spacing ";
    functions += "spread sum system_clock tiny transfer transpose trim ";
    functions += "ubound unpack verify ";

    // Fortran 2003 intrinsic functions
    functions += "c_associated c_f_pointer c_f_procpointer c_funloc c_loc ";
    functions += "command_argument_count get_command get_command_argument ";
    functions += "get_environment_variable is_iostat_end is_iostat_eor ";
    functions += "move_alloc new_line ";

    // GNU extensions
    functions += "abort access acosh alarm and asinh atanh besj0 besj1 "
    functions += "besjn besjn besy0 besy1 besyn besyn chdir chmod ctime ";
    functions += "dcmplx dfloat erf erfc etime exit fdate fget fgetc flush ";
    functions += "fnum fputc fput free fseek fstat ftell gamma gerror ";
    functions += "getarg getcwd getenv getgid getlog getpid getuid ";
    functions += "gmtime hostnm iargc idate ierrno imagpart int2 int8";
    functions += "irand isatty isnan itime kill lgamma link lnblnk loc ";
    functions += "long lshift lstat ltime malloc mclock mclock8 or perror ";
    functions += "ran rand realpart rename rshift secnds second short ";
    functions += "signal sizeof sleep srand stat symlnk system time time8 ";
    functions += "ttynam umask unlink xor ";

    // f2c extensions
    functions += "imag zabs zcos zexp zlog zsin zsqrt ";

    // operators
    var operators = "==|&lt;=|&gt;=|&lt;|&gt;|\/=|\\+|-|\/|\\*|\\*\\*|\/|";
    operators += "\\.eq\\.|\\.ne\\.|\\.lt\\.|\\.le\\.|\\.gt\\.|";
    operators += "\\.ge\\.|\\.not\\.|\\.and\\.|\\.or\\.|";
    operators += "\\.eqv\\.|\\.neqv\\.|\\.true\\.|\\.false\\.";

    // misc symbols
    var symbols = "=|::|:|\\(|\\)|,|&amp;|%|\\[|\\]|;";

    // literal number (integer and float)
    // FIXME: This also matches numbers in var names 
    var literal_num = "\[+-\]?\\d+(\\.\\d+([eEdD][+-]?\\d+)?)?";

    // map pattern to CSS style
    this.regexList = [
	{ regex: new RegExp("^C(.*)$","gm"), css:'comments' },
	{ regex: new RegExp("!(.*)$","gm"), css:'comments' },
	{ regex: SyntaxHighlighter.regexLib.doubleQuotedString,	css:'string' },
	{ regex: SyntaxHighlighter.regexLib.singleQuotedString,	css:'string' },
	{ regex: new RegExp(this.getKeywords(datatypes), 'gmi'), css:'keyword' },
	{ regex: new RegExp(this.getKeywords(keywords), 'gmi'), css:'keyword' },
	{ regex: new RegExp(this.getKeywords(functions), 'gmi'), css:'functions' },
	{ regex: new RegExp(symbols, 'gmi'), css:'color1 bold' },
	{ regex: new RegExp(operators, 'gmi'), css:'color2 bold' },
	{ regex: new RegExp(literal_num, 'gm'), css:'value bold' }
    ];

  };

  Brush.prototype = new SyntaxHighlighter.Highlighter();
  Brush.aliases	= ['fortran'];

  SyntaxHighlighter.brushes.Fortran = Brush;

  // CommonJS
  typeof(exports) != 'undefined' ? exports.Brush = Brush : null;
}) ();
