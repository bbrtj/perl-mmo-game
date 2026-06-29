use v5.42;
use experimental 'class';

class My::PQ;

use Algorithm::Heapify::XS qw(min_heap_push min_heap_adjust_top);

field @heap;

method top ()
{
	return $heap[0];
}

method extract_top ()
{
	my $el = shift @heap;
	min_heap_adjust_top(@heap);
	return $el;
}

method add ($el)
{
	min_heap_push(@heap, $el);
	return;
}

method size ()
{
	return scalar @heap;
}

