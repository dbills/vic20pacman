#generate maze bitstring
# col * rows at two bytes, + line ending characters
use Switch;

$left_top="/";
$left_bot="L";
$right_bot="R";
$right_top="\\";
$tee_top="T";   
$left_cap="[";
$right_cap="]";
$top_cap="^";
$bot_cap="v";
$tee_left="}";
$tee_right="{";
$hwall="=";
$vwall="|";
$power="*";


@LINES=<STDIN>;
chop(@LINES);
chop(@LINES);
foreach(@LINES) {
    $len = length($_);
    $len==22 || die("line length wrong $len:\n$_\n");
    for($i=0;$i<$len;$i++) {
        $char=substr($_,$i,1);
        switch($char) {
            case "#" { $output=$output."001" }
            case "." { $output=$output."010" }
            case "*" { $output=$output."011" }
            case " " { $output=$output."000" }
            case "=" { $output=$output."100" }
            else { die("huh?"); }
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
