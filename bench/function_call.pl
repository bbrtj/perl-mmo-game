use v5.42;

use Benchmark::Dumb qw(cmpthese);

package MethodBench {

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

cmpthese 200.01, {
	'method' => sub {
		MethodBench->method_call(1, 2, 3);
	},
	'func' => sub {
		func_call(1, 2, 3);
	},
	'method + sum' => sub {
		MethodBench->method_sum(1, 2, 3);
	},
	'func + sum' => sub {
		func_sum(1, 2, 3);
	},
};

