package i18n;

use header;
use Exporter qw(import);
use Mojo::File qw(curfile);
use Data::Localize;

our @EXPORT = qw(
	_t
);

sub _t ($message, @args)
{
	state $localizer = do {
		my $loc = Data::Localize->new(auto => 1);

		$loc->add_localizer(
			class => 'Gettext',
			path => curfile->dirname->to_string . '/i18n/*.po',
		);
		$loc->set_languages;
		$loc;
	};

	return $localizer->localize($message, @args);
}

1;
