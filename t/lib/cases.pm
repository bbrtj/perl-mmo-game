package cases;

require Test::More;

use header -noclean;

sub get_sub ($desc, @cases)
{
	return sub : prototype(&) ($tester) {
		Test::More::subtest $desc, sub {
			my $num = 0;
			for my $case (@cases) {
				local $_ = "case $num ok";
				my $output = $tester->($case->@*);
				$num += 1;
			}
		};
	};
}

sub import ($class, %cases)
{
	my $pkg = caller;

	for my $desc (keys %cases) {
		croak 'invalid test name'
			unless $desc =~ m/[a-z0-9 _]/;

		my @cases = $cases{$desc}->@*;
		my $subname = $desc =~ s/ /_/gr;

		{
			no strict 'refs';
			*{"${pkg}::test_${subname}"} = get_sub $desc, @cases;
		}
	}
}

