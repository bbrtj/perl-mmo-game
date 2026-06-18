use experimental 'class';

class Resource::ActorsInfo :isa(Resource);

use header;

use constant type => 'actors_info';

field $actors :param(subject);    # array of Unit::Actor

method generate ()
{
	# TODO: more basic info
	my @actors_data = map {
		{
			id => $_->id,
			name => $_->character->name,
			class => $_->character->class_id,
			player => $_->is_player,
		}
	} $actors->@*;

	return \@actors_data;
}

