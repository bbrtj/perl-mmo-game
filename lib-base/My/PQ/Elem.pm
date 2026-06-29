use v5.42;
use experimental 'class';

class My::PQ::Elem;

field $val :reader :param;
field $cmp_val :reader :param;

use overload
	'<=>' => 'compare_val',
	fallback => true;

method compare_val ($other, $)
{
	return $cmp_val <=> $other->cmp_val;
}

