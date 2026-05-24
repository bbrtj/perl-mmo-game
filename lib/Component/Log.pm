package Component::Log;

use My::Moose;
use Log::Handler;
use Time::Piece;

use header;

with 'Component::Role::HasEnv';

has param 'logger' => (
	isa => Types::InstanceOf ['Log::Handler'],
	lazy => sub ($self) {
		Log::Handler->new(@{$self->build_config});
	},
	handles => [qw(debug info warning error critical emergency)],
);

has param 'filename' => (
	is => 'rw',
	isa => Types::Str,
);

has option 'system_name' => (
	is => 'rw',
	isa => Types::Str,
);

sub _get_log_callback ($self)
{
	return sub ($params) {
		my $time = localtime;
		my $time_str = $time->ymd . ' ' . $time->hms;

		my $str = "[$time_str] ";
		my $level_str = uc $params->{level};
		chomp $params->{message};
		my $ph = " " x length $str;
		$params->{message} =~ s/(\R)/$1$ph\[$level_str] /g;

		$params->{message} = "$str\[$level_str] $params->{message}\n";
	};
}

sub _get_screen_callback ($self)
{
	return sub ($params) {
		my $time = localtime;
		my $time_str = $time->hms;
		my $sys_str = $self->system_name // '';

		$sys_str = "[$sys_str] " if $sys_str;
		my $str = "[$time_str] $sys_str";
		my $level_str = uc $params->{level};
		chomp $params->{message};
		my $ph = " " x length $str;
		$params->{message} =~ s/(\R)/$1$ph\[$level_str] /g;

		$params->{message} = "$str\[$level_str] $params->{message}\n";
	};
}

sub build_config ($self)
{

	return [
		(
			$self->env->is_production ? () : (
				screen => {
					log_to => 'STDOUT',
					'utf-8' => true,
					maxlevel => 'debug',
					minlevel => 'critical',
					message_layout => '%m',
					message_pattern => [qw(%L %m)],
					prepare_message => $self->_get_screen_callback,
				}
			)
		),
		file => {
			maxlevel => 'warning',
			filename => $self->filename,
			'utf-8' => true,
			message_layout => '%m',
			message_pattern => [qw(%L %m)],
			prepare_message => $self->_get_log_callback,
		},
		screen => {
			log_to => 'STDERR',
			'utf-8' => true,
			maxlevel => 'emergency',
			message_layout => '%m',
			message_pattern => [qw(%L %m)],
			prepare_message => $self->_get_log_callback,
		},
	];
}

