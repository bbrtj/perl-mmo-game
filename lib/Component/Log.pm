package Component::Log;

use Types;
use My::Moose;
use Log::Dispatch;
use Time::Piece;

use header;

has 'logger' => (
	is => 'ro',
	isa => Types::InstanceOf ['Log::Dispatch'],
	lazy => 1,
	default => sub ($self) {
		Log::Dispatch->new(outputs => $self->build_config);
	},
	handles => [qw(debug info warning error critical emergency)],
);

has 'filename' => (
	is => 'rw',
	isa => Types::Str,
);

sub build_config ($self)
{
	my $callback = sub (%params) {
		my $time = localtime;
		my $time_str = $time->ymd . ' ' . $time->hms;

		my $str = "[$time_str] ";
		my $level_str = uc $params{level};
		chomp $params{message};
		my $ph = " " x length $str;
		$params{message} =~ s/(\R)/$1$ph\[$level_str] /g;

		return "$str\[$level_str] $params{message}\n";
	};

	return [
		[
			'Screen',
			name => 'stdout',
			min_level => 'debug',
			max_level => 'warning',
			stderr => 0,
			callbacks => $callback,
		],
		[
			'File::Locked',
			name => 'file',
			min_level => 'warning',
			filename => $self->filename,
			mode => '>>',
			binmode => ":encoding(UTF-8)",
			callbacks => $callback,
		],
		[
			'Screen',
			name => 'stderr',
			min_level => 'error',
			callbacks => $callback,
		],
	];
}
