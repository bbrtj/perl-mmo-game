use v5.42;

use Benchmark::Dumb qw(cmpthese);
use experimental 'class';

class MethodBench {

	method method_call ($a, $b, $c)
	{
		return;
	}

	method method_sum ($a, $b, $c)
	{
		return $a + $b + $c;
	}
}

package BlessMethodBench {

	sub method_call ($self, $a, $b, $c)
	{
		return;
	}

	sub method_sum ($self, $a, $b, $c)
	{
		return $a + $b + $c;
	}
}

sub func_call ($a, $b, $c)
{
	return;
}

sub func_sum ($a, $b, $c)
{
	return $a + $b + $c;
}

my $obj_bless = bless {}, 'BlessMethodBench';
my $obj_class = MethodBench->new;

cmpthese 200.01, {
	'static' => sub {
		BlessMethodBench->method_call(1, 2, 3);
	},
	'static sum' => sub {
		BlessMethodBench->method_sum(1, 2, 3);
	},
	'method' => sub {
		$obj_bless->method_call(1, 2, 3);
	},
	'method sum' => sub {
		$obj_bless->method_sum(1, 2, 3);
	},
	'class' => sub {
		$obj_class->method_call(1, 2, 3);
	},
	'class sum' => sub {
		$obj_class->method_sum(1, 2, 3);
	},
	'func' => sub {
		func_call(1, 2, 3);
	},
	'func sum' => sub {
		func_sum(1, 2, 3);
	},
};

