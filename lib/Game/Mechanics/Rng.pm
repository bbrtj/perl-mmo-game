package Game::Mechanics::Rng;

use Exporter qw(import);
use Crypt::PRNG qw(rand);
use Array::Sample::WeightedRandom qw(sample_weighted_random_no_replacement);

use header;

our @EXPORT = qw(
	rng
);

our @EXPORT_OK = qw(
	random_number
	random_int
	random_choice
	weighted_choice
);

use constant USES_RANDOM => !$ENV{TEST_NO_RANDOM};

sub rng
{
	if (USES_RANDOM) {
		goto \&rand;
	}
	else {
		return 1;
	}
}

sub random_number ($min, $max)
{
	return (($max - $min) * rng) + $min;
}

sub random_int ($min = 0, $max = 100)
{
	return int random_number $min, $max;
}

sub random_choice ($items, $count = 1)
{
	return weighted_choice([map { [$_, 1] } $items->@*], $count);
}

sub weighted_choice ($items, $count = 1)
{
	return sample_weighted_random_no_replacement($items, $count);
}

