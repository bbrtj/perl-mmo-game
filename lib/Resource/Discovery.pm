use experimental 'class';

class Resource::Discovery :isa(Resource);

use Resource::ActorPosition;
use Resource::ActorState;
use Sub::Install;

use header;

use constant type => 'discovery';

field %aspects;

my @aspect_keys = qw(
	new_actors
	old_actors
);

# NOTE: aspects are full objects
foreach my $aspect (@aspect_keys) {
	Sub::Install::install_sub(
		{
			code => method($list) {
				$aspects{$aspect} = $list;
			},
			as => $aspect,
		}
	);
}

method generate ()
{
	my %generated;

	foreach my ($key, $value) (%aspects) {
		$generated{$key} = [
			map {
				$_->id
			} $value->@*
		];
	}

	return \%generated;
}

method _build_next_resources ()
{
	my @resources;
	foreach my $actor (($aspects{new_actors} // [])->@*) {
		push @resources, (
			Resource::ActorPosition->new(subject => $actor),
			Resource::ActorState->new(subject => $actor),
		);
	}

	return \@resources;
}

