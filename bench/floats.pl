use v5.42;
no warnings 'void';
use Benchmark::Dumb qw(cmpthese);
use Utils qw(transport_float transport_floats);
# use Inline 'C';

my @floats = map { rand() * 100 } 1 .. 6;

# sub check ($subname, $value, $expected)
# {
# 	my $sub = \&$subname;
# 	my $result = $sub->($value);

# 	die "bad $subname assumption: $result, not $expected"
# 		unless $result eq $expected;
# }

# check('my_sprintf_float', 0.123412345, '0.1234');
# check('my_sprintf_int', 0.123412345, '1234');
# check('my_sprintf_float_c', 0.123412345, '0.1234');
# check('my_sprintf_int_c', 0.123412345, '1234');
# check('transport_float_c', 0.123412345, '1234');
# check('transport_float_c', -0.123412345, '-1234');
# check('transport_float_c', -0, '0');
# check('transport_float_c', -50051.99929, '-500519992');

cmpthese(
	200.01, {
		float => sub {
			"$_" for @floats;
		},
		int => sub {
			transport_float $_ for @floats;
		},
		ints => sub {
			transport_floats @floats;
		},
		# sprintf_float_c => sub {
		# 	my_sprintf_float_c($_) for @floats;
		# },
		# sprintf_int_c => sub {
		# 	my_sprintf_int_c($_) for @floats;
		# },
		# int_c => sub {
		# 	transport_float_c($_) for @floats;
		# },
	}
);

__DATA__

__C__

#define BUFLEN 20

SV* my_sprintf_int_c (double value)
{
	long long value_int = value * 10000;
	char buffer[BUFLEN];
	int size = sprintf(buffer, "%d", value_int);

	return newSVpv(buffer, size);
}

SV* my_sprintf_float_c (double value)
{
	char buffer[BUFLEN + 10];
	int size = sprintf(buffer, "%.4f", value);

	return newSVpv(buffer, size);
}

SV* transport_float_c (double value)
{
	long long value_int = abs(value * 10000);
	char buffer[BUFLEN];
	int i = BUFLEN;

	if (value_int == 0)
		return newSVpv("0", 1);

	while (value_int > 0) {
		long long rem = value_int % 10;
		buffer[--i] = 48 + rem;
		value_int = value_int / 10;
	}

	if (value < 0)
		buffer[--i] = '-';

	/* int size = sprintf(buffer, "%d", value_int); */

	return newSVpv(buffer + i, BUFLEN - i);
}

