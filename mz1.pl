#generate maze bitstring
# col * rows at two bytes, + line ending characters
use Switch;

$power2    ="0000"; #4
$power     ="0000"; #5
$dot       ="0000"; #6
$tee_bot   ="0001";
$hwall     ="0010";   
$vwall     ="0011";   
$left_top  ="0100";
$right_top ="0101";
$left_bot  ="0110";
$right_bot ="0111";
$top_cap   ="1000";
$bot_cap   ="1001";
$left_cap  ="1010";
$right_cap ="1011";
$tee_top   ="1100";   
$tee_left  ="1101";
$tee_right ="1110";
$space     ="1111";


@LINES=<STDIN>;
chop(@LINES);
chop(@LINES);
foreach(@LINES) {
    $len = length($_);
    $len==22 || die("line length wrong $len:\n$_\n");
    for($i=0;$i<$len;$i++) {
        $char=substr($_,$i,1);
        switch($char) {
            case "/" { $output=$output.$left_top;}
            case "L" {$output=$output.$left_bot;}
            case "R" {$output=$output.$right_bot;}
            case "\\" {$output=$output.$right_top;}
            case "T" {$output=$output.$tee_top;}
            case "[" {$output=$output.$left_cap;}
            case "]" {$output=$output.$right_cap;}
            case "^" {$output=$output.$top_cap;}
            case "v" {$output=$output.$bot_cap;}
            case "}" {$output=$output.$tee_left;}
            case "-" {$output=$output.$tee_bot;}
            case "{" {$output=$output.$tee_right;}
            case "=" {$output=$output.$hwall;}
            case "|" {$output=$output.$vwall;}
#            case "*" {$output=$output.$power;}
            case "*" {$output=$output.$space;}
            case "." {$output=$output.$dot; }
            case "*" {$output=$output.$power; }
            case " " {$output=$output.$space; }
            else { die("huh? - $char -"); }
        }
    }
}
$len = length($output);
print ";;len = $len\n";
$output=$output."00";
print "MazeB\n";
for($i=0;$i<length($output);$i++) {
    if($i%8==0) {
        print "\n    dc.b %";
    }
    print substr($output,$i,1);
        
}
print "\nMazeX\n";
#print "\n$output\n";
