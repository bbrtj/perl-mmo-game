package Test2::Tools::Provider;

use Exporter qw(import);
use Test2::Tools::Subtest qw(subtest_buffered);

use header;

our @EXPORT = qw(test test_data before_each);

my $before;
my %subs;

sub before_each : prototype(&) ($sub)
{
	$before = $sub;

	return;
}

sub get_sub ($desc, @cases)
{
	return sub ($tester) {
		subtest_buffered $desc, sub {
			my $num = 0;
			foreach my $case (@cases) {
				$before->($num)
					if defined $before;

				local $_ = "case $num ok";
				my $output = $tester->($case->@*);
				$num += 1;
			}
		};
	};
}

sub test_data (%cases)
{
	foreach my $desc (keys %cases) {
		croak "invalid test name $desc"
			unless $desc =~ m/[a-z0-9 _]/;

		my @cases = $cases{$desc}->@*;
		my $subname = lc $desc =~ s/ /_/gr;

		croak "test name $desc already exists"
			if $subs{$subname};

		$subs{$subname} = get_sub $desc, @cases;
	}

	return;
}

sub test ($name, $sub)
{
	croak 'no such test name'
		unless exists $subs{$name};

	$subs{$name}->($sub);
	return;
}

