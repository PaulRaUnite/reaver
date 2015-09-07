#!/usr/bin/perl

my $argc = 0;
my $srcfile;
my $unwind = 0;
foreach my $arg (@ARGV) {
  if($argc==0) { $srcfile=$arg; }
  $argc++;
}
if($argc!=1) { die "usage: <srcfile>\n"; }

my $infilename = (join '',($srcfile,".backup"));
my $outfilename = $srcfile;
system("cp","-f",$outfilename,$infilename);
open $infile, $infilename;
open $outfile, (join '',(">",$outfilename));
while (my $line = <$infile>) {
#  print "line: ", $line, "\n";
  if($line =~ m/jordansage (.*);/) {
    my $j = $1;
#    print "pattern: ", $j, "\n";
    $j =~ s/,\[//g;
    $j =~ s/,/ /g;
    $j =~ s/\[//g;
    $j =~ s/\]/\n/g;
    $j =~ s/#/\n/g;
    open $minfile, ">m.in";
    print $minfile $j;
    close($minfile);
   system("sage","-python", "jordan.sage", "m.in", "m.out");
#    system("sage","-python", "../Aditya/jordan-sample.sage", "m.in", "m.out");
    print $outfile "  jordansage\n";
    open $moutfile, "m.out";
    my $oldline = "";
    my $s;
    my $sinv;
    my $jordan;
    while (my $mline = <$moutfile>) {
      if(($oldline =~ m/Sinv(.*)/) && ($mline =~/(.*)/)) {
        my $m = $1;
        $m =~ s/ //g;
        $sinv = join '', ("    matrix(",$m,")\n");
      }
      elsif(($oldline =~ m/S(.*)/) && ($mline =~/(.*)/)) {
        my $m = $1;
        $m =~ s/ //g;
        $s = join '', ("    matrix(",$m,")\n");
      }
      elsif(($oldline =~ m/Jordan/) && ($mline =~/(.*)/)) {
        my $m = $1;
        $m =~ s/ //g;
        $jordan = join '', ("    ",$m,"\n");
      }
      $oldline = $mline;
    }
    close($moutfile);
    print $outfile $s, $jordan, $sinv;
    print $outfile "    ;\n";
  }
  else {
    print $outfile $line;
  }
}
close($infile);
close($outfile);
