package Game::RNG;

use Exporter qw(import);
use Data::Entropy::Algorithms qw(rand_flt rand_int pick_r);
use Quantum::Superpositions::Lazy;

use header;

our @EXPORT = qw(
	rng
);

our @EXPORT_OK = qw(
	random_number
	random_choice
	weighted_choice
);

sub rng ()
{
	return rand_flt 0, 1;
}

sub random_number ($min = 0, $max = 100)
{
	return rand_int($max - $min) + $min;
}

sub random_choice ($items)
{
	die 'random_choice expects an array reference'
		unless ref $items eq ref [];
	return pick_r($items);
}

sub weighted_choice ($items_with_weights)
{
	die 'weighted_choice expects an array reference'
		unless ref $items_with_weights eq ref [];

	return superpos($items_with_weights)->collapse;
}

