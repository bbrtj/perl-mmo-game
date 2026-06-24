use experimental 'class';

class Resource::Discovery :isa(Resource);

use Resource::ActorPosition;
use Resource::ActorState;
use Sub::Install;

use header;

use constant type => 'discovery';

field %aspects;
field %objects;
field $should_send :reader = false;

my @aspect_keys = (
	{
		method => 'add_new_actor',
		key => 'new_actors',
		object => true,
	},
	{
		method => 'add_old_object',
		key => 'old_objects',
		object => false,
	},
);

# NOTE: aspects are full objects
foreach my $aspect (@aspect_keys) {
	my $key = $aspect->{key};

	my $method = $aspect->{object}
		? method($item) {
			push $aspects{$key}->@*, $item->id;
			push $objects{$key}->@*, $item;
			$should_send = true;
		}
		: method($item) {
		push $aspects{$key}->@*, $item;
		$should_send = true;
		};

	Sub::Install::install_sub(
		{
			code => $method,
			as => $aspect->{method},
		}
	);
}

method generate ()
{
	return {%aspects};
}

method _build_next_resources ()
{
	my @resources;
	foreach my $actor (($objects{new_actors} // [])->@*) {
		push @resources, (
			Resource::ActorPosition->new(subject => $actor),
			Resource::ActorState->new(subject => $actor),
		);
	}

	return \@resources;
}

