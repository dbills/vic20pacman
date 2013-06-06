#generate maze bitstring
# col * rows at two bytes, + line ending characters
use Switch;

$power2    ="0000"; #4
$power     ="0000"; #5
# char #6 starts here
$right_top ="0";
$tee_right ="1";
$right_cap ="2";
$right_bot ="3";
$left_top  ="4";

$tee_left  ="5";
$left_cap  ="6";
$left_bot  ="7";
$dot       ="8"; 

$tee_bot   ="9";
$hwall     ="a";   
$vwall     ="b";   
$top_cap   ="c";
$bot_cap   ="d";
$tee_top   ="e";   
$space     ="f";

$skip=<STDIN>;#eat first line
@LINES=<STDIN>;
chop(@LINES);
#chop(@LINES);
foreach(@LINES) {
    $len = length($_);
    $len==22 || die("line length wrong $len:\n$_\n");
#    for($i=0;$i<$len;$i++) {
    for($i=1;$i<12;$i++) {
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
$bytes=0;
for($i=0;$i<length($output);$i++) {
    if($i%2==0) {
        if($i>0) {
            $bytes++;
            if($bytes==11) {
                print "\n    dc.b \$";
                $bytes=0;
            } else {
                print ",\$";
            }
        } else {
            print "\n    dc.b \$";
        }
    }
    print substr($output,$i,1);
        
}
print "\nMazeX\n";
#print "\n$output\n";
