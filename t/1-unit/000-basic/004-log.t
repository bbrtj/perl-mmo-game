use testheader;

use Component::Log;
use Component::Env;
use Path::Tiny qw(tempfile);
use Capture::Tiny qw(capture);
use Encode qw(decode);

my $file = tempfile('gameXXXXXXX');
my $log = Component::Log->new(
	filename => $file->stringify,
	env => Component::Env->new,
);

my ($stdout, $stderr) = capture {
	$log->warning('zażółć gęślą jaźń');
	$log->error("test2\ntest3");
};

my $logged = $file->slurp({binmode => 'encoding(UTF-8)'});
$stdout = decode 'UTF-8', $stdout;
$stderr = decode 'UTF-8', $stderr;
my $date_re = qr{\[[\d: -]+\]};

foreach my $output (['file', $logged], ['screen', $stdout]) {
	subtest "should log to $output->[0]" => sub {
		my $string = $output->[1];

		like $string, qr{ ^ $date_re \s \[WARNING\] \s zażółć \s gęślą \s jaźń $ }mx, 'first line ok';
		like $string, qr{ ^ $date_re \s \[ERROR\] \s test2 $ }mx, 'second line ok';
		like $string, qr{ ^ \s+ \[ERROR\] \s test3 $ }mx, 'third line ok';
	};
}

done_testing;

