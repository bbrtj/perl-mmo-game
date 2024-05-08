package CLI::prepare_client;

use My::Moose -constr;

use CLI::export_config;
use CLI::export_mo;
use CLI::export_lore;
use CLI::export_maps;

use header;

extends 'Mojolicious::Command';

use constant description => 'Export everything for the client program at once';
sub usage ($self) { return $self->extract_usage }

sub run ($self, $language = undef)
{
	unless (defined $language) {
		$self->help;
		return;
	}

	say 'Exporting config';
	CLI::export_config->new->run();

	say 'Exporting lore';
	CLI::export_lore->new->run($language);

	say 'Exporting maps';
	CLI::export_maps->new->run();

	say 'Exporting translations';
	CLI::export_mo->new->run($language);

	say 'All done';
	return;
}

__END__

=head1 SYNOPSIS

	Usage: APPLICATION prepare-client [LANGUAGE]

