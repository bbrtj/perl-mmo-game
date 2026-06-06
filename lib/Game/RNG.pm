package Game::RNG;

use Exporter qw(import);
use Crypt::PRNG qw(rand);
use Quantum::Superpositions::Lazy;

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

sub rng
{
	return rand;
}

sub random_number ($min, $max)
{
	return (($max - $min) * rng) + $min;
}

sub random_int ($min = 0, $max = 100)
{
	return int random_number $min, $max;
}

sub random_choice ($items)
{
	die 'random_choice expects an array reference'
		unless ref $items eq ref [];

	return weighted_choice([map { [1, $_] } $items->@*]);
}

sub weighted_choice ($items_with_weights)
{
	die 'weighted_choice expects an array reference'
		unless ref $items_with_weights eq ref [];

	return superpos($items_with_weights)->collapse;
}

