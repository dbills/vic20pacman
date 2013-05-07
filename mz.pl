#generate maze bitstring
# col * rows at two bytes, + line ending characters
$sz = 22*23*2+(23);
$bytes_read = read(STDIN,$MAZE,$sz);
$bytes_read==$sz || die("incorrect maze size\n");
print "bytes read $bytes_read\n";
#print "mz = $MAZE\n";
$MAZE=~s/\n//g;
#print "mz = $MAZE\n";
$output="";
for($i=0;$i<length($MAZE);$i+=2) 
{
    $char = substr($MAZE,$i,1);
    if($char eq "#") {
        $output=$output."001";
    } elsif($char eq ".") {
        $output=$output."010";
    } elsif($char eq ">") {
        $output=$output."010";
    } elsif($char eq "<") {
        $output=$output."011";
    } elsif($char eq "^") {
        $output=$output."100";
    } elsif($char eq "-") {
        $output=$output."101";
    } elsif($char eq "=") {
        $output=$output."110";
    } elsif($char eq "|") {
        $output=$output."111";
    } elsif($char eq " ") {
        $output=$output."000";
    }
}
print "MazeB\n"; 
print "    dc.b 1,%";
for($i=0;$i<length($output);$i++) {
    if($i%8==0 && $i!=0) {
        print "\n    dc.b 1,%";
    }
    print substr($output,$i,1);
}
print "\n";
# ;; DOT
# ;; POWER
# ;; LEFT
# ;; RIGHT
# ;; TOP
# ;; BOTTOM
# ;; H WALL
# ;; V WALL
