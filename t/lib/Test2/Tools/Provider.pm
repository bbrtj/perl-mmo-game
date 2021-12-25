package Test2::Tools::Provider;

use Exporter qw(import);
use Test2::Tools::Subtest qw(subtest_buffered);

use header -noclean;

our @EXPORT = qw(test_data before_each);

my $before;

sub before_each : prototype(&) ($sub)
{
	$before = $sub;
}

sub get_sub ($desc, @cases)
{
	return sub : prototype(&) ($tester) {
		subtest_buffered $desc, sub {
			my $num = 0;
			for my $case (@cases) {
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
	my $pkg = caller;

	for my $desc (keys %cases) {
		croak 'invalid test name'
			unless $desc =~ m/[a-z0-9 _]/;

		my @cases = $cases{$desc}->@*;
		my $subname = lc $desc =~ s/ /_/gr;

		{
			no strict 'refs';
			*{"${pkg}::test_${subname}"} = get_sub $desc, @cases;
		}
	}

	return;
}
