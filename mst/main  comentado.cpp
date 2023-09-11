/* main.cpp -- This file contains the MST entry point.
 *
 * Copyright (C) 2018-2020 "AZ Company Group" LLC <https://gkaz.ru/>
 * Copyright (C) 2018-2020 Artyom V. Poptsov <a@gkaz.ru>
 * Copyright (C) 2018-2019 Anton Plekhanov <plehunov.anton9@gmail.com>
 * Copyright (C) 2019 Daniil Zemlyanoy <lfybssd@gmail.com>
 *
 * This file is part of MST.
 *
 * MST is free software: you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * MST is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with MST.  If not, see <http://www.gnu.org/licenses/>.
 */

//importação de biblioteca
#include "ui/install_window/installwindow.h" //Essa biblioteca inclui o cabeçalho (header) para uma janela de instalação (ou outra interface de usuário) que parece ser parte do aplicativo em questão. Esse arquivo de cabeçalho contém a definição da classe InstallWindow ou de outras classes relacionadas à interface gráfica.
#include <QApplication> //Essa é uma biblioteca da Qt que fornece funcionalidades relacionadas à criação e gerenciamento de aplicativos gráficos. Ela contém a classe QApplication, que é frequentemente usada como ponto de entrada para aplicativos Qt e lida com a inicialização do aplicativo gráfico.
#include <QFile>
#include <QDir>
#include <QScopedPointer>
#include <QTextStream>
#include <QDateTime>
#include <QLoggingCategory>
#include <unistd.h>
#include <QDebug>
#include <QFile>
#include <QString>
#include <QTextStream>
#include <QCommandLineParser>
#include <QTranslator>
#include "config.h"
#include "core/template_manager.h"
#include "core/platform.h"

//permite o uso dos elementos do namespace standard
using namespace std;

/**
 * @brief m_logFile -- A pointer to logging file.
 */
QScopedPointer<QFile>   m_logFile;

/**
 * @brief MST_CONFIG_FILE -- The default MST configuration file.
 */

//declaração de constante 
const QString MST_CONFIG_FILE = "/etc/mst";

const QString MST_SEATS_CONFIG_FILE = "/etc/mst-seats";

/**
 * @brief MST_LOG_FILE -- The default MST logging file.
 */

//declaração de constante
const QString MST_LOG_FILE   = "/var/log/mst.log";

/**
 * @brief MST_TEMPLTE_DIR -- Template directory path.
 */
//declaração de constante
const QString MST_TEMPLATE_DIR = INSTALLATION_PREFIX "/var/lib/mst/";

/**
 * @brief messageHandler -- The MST default logging handler.
 * @param type -- A log message type.
 * @param context -- A log message context.
 * @param msg -- A log message.
 */

//inicio de uma função que não há nenhum retorno
//nome da função é messageHandler, e essa função posui três parametros
//essa função atua como manipulador de mensagem de log
//essa função é requisitada quando há mensagem de log a ser registrada e formata a mensagem de acordo com o típo de mensagem
//QtMsgType type: Representa o tipo de mensagem de log, como informação, depuração, aviso, erro ou fatal. O programa pode usar esse parâmetro para determinar o nível de gravidade da mensagem de log.
//const QMessageLogContext &context: Referência constante a um objeto QMessageLogContext. Ele fornece informações de contexto sobre a mensagem de log, como o nome da categoria em que a mensagem foi registrada e informações sobre a origem da mensagem.
//const QString &msg: Referência constante a uma string QString. Ele contém o texto da mensagem de log em si.
void messageHandler(QtMsgType type, const QMessageLogContext &context,
                    const QString &msg)
{
//cria um objeto QTextStream  chamado out que é inicializado para um arquivo de ponteiro
//m_logFile.data
    QTextStream out(m_logFile.data());
//insere na variavel out a data  atual
    out << QDateTime::currentDateTime().toString("yyyy-MM-dd hh:mm:ss.zzz ");
//inicia uma estrutura de seleção
    switch (type)
    {
    case QtInfoMsg:     out << "INF "; break;
    case QtDebugMsg:    out << "DBG "; break;
    case QtWarningMsg:  out << "WRN "; break;
    case QtCriticalMsg: out << "CRT "; break;
    case QtFatalMsg:    out << "FTL "; break;
    }

    out << context.category << ": " << msg << Qt::endl;
    out.flush();
}

//função cujo objetivo é criar um arquivo de configuração 
void create_config_file(QFile& file) {
    if (! file.open(QIODevice::WriteOnly | QIODevice::Text))
            return;
    QTextStream out(&file);
    out << "user:multiseat";
}

/**
 * @brief list_backups -- Print the list of backups to the stdout.
 * @param mst -- A MST instance to use.
 */

//função que lista os backups
static void list_backups(const MST* mst) {
    cout << "Backup directory: "
         << mst->get_backup_directory().toStdString()
         << endl;
    for (QString s : mst->list_backups()) {
        if (mst->list_backups().first() == s) {
            cout << "* ";
        } else {
            cout << "  ";
        }
        cout << s.toStdString() << endl;
    }
}

/**
 * @brief main -- The application entry point.
 * @param argc -- Count of program arguments.
 * @param argv -- Array of program arguments.
 * @return A result of execution (0 means OK, non-zero value means that some
 *     error occured.)
 */

//inicio do programa principal
int main(int argc, char *argv[])
{

//um arquivo de log aberto para a gravação em modo de anexação 
    m_logFile.reset(new QFile(MST_LOG_FILE));
    m_logFile.data()->open(QFile::Append | QFile::Text);

//um manipulador de mensagem de log é instalado usando a função qInstallMessagemHandler
    qInstallMessageHandler(messageHandler);

//um objeto de aplicativo qt denominado a 
    QSharedPointer<QCoreApplication> a;
    bool is_graphic_mode = platform::is_graphics_available();

//se um modulo grafico estiver disponivel o objeto criado é "Qapplication".Caso contrario o objeto sera QcoreApplication
    if (is_graphic_mode) {
        a.reset(new QApplication(argc, argv));
    } else {
        a.reset(new QCoreApplication(argc, argv));
    }

//um objeto de tradução qtranslator é criado
    QTranslator translator;
//esse objeto criado é carregado com traduções para o idioma do sistema
    bool ok = translator.load(":/i18n/mst_" + QLocale().system().name());
//se a tradução for carregada ela é instalada no aplicativo 
    if (ok) {
        a->installTranslator(&translator);
   } else {
        cerr << "ERROR: Could not load translations" << endl;
    }

//criação de uma instancia do qcomandlineParser.esta instancia é um classe de biblioteca qt usado para processar e gerenciar opções de linhas de comando para o programa 
    QCommandLineParser parser;
//definição de uma descrĩção para o aplicativo que é mostrado ao exibir ajuda 
    parser.setApplicationDescription("Multiseat configurator");
//adhelpoption e adversionoption adicionam opções padrão de ajuda e versão á linha de comando 
    parser.addHelpOption();
    parser.addVersionOption();

//é definida a opção list_backups_option onde o nome de opção de linha de comando é list-backups
    QCommandLineOption list_backups_option(
                QStringList() << "list-backups",
                QCoreApplication::translate("main",
                                           "Show the list of system backups"));

//é definida a opção rollback_option onde o nome de opção de linha de comando é R e rollback
    QCommandLineOption rollback_option(
                QStringList() << "R" << "rollback",
                QCoreApplication::translate("main",
                                           "Rollback changes in the system"));

//é definida a opção start_option onde o nome de opção de linha de comando é s e start
    QCommandLineOption start_option(
                QStringList() << "s" << "start",
                QCoreApplication::translate("main",
                                            "Start mstd."));

//é definida a opção stop_option onde o nome de opção de linha de comando é S e stop
    QCommandLineOption stop_option(
                QStringList() << "S" << "stop",
                QCoreApplication::translate("main",
                                            "Stop mstd."));

//é definida a opção status_option onde o nome de opção de linha de comando é status
    QCommandLineOption status_option(
                QStringList() << "status",
                QCoreApplication::translate("main",
                                            "Show MST status"));

//é definida a opção debug_allow_empty_devices onde o nome de opção de linha de comando é debug-allow-empty-devices
    QCommandLineOption debug_allow_empty_devices(
                QStringList() << "debug-allow-empty-devices",
                QCoreApplication::translate("main",
                                            "Allow empty devices"));

//é definida a opção debug_allow_devices_collisions onde o nome de opção de linha de comando é debug-allow-device-collisions
    QCommandLineOption debug_allow_device_collisions(
                QStringList() << "debug-allow-device-collisions",
                QCoreApplication::translate("main",
                                            "Allow device collisions"));

//é definida a opção debug_allow_missing_componentes onde o nome de opção de linha de comando é debug-allow-missing-components
    QCommandLineOption debug_allow_missing_components(
                QStringList() << "debug-allow-missing-components",
                QCoreApplication::translate("main",
                                            "Allow missing components."));

//é definida a opção list_input_devices onde o nome de opção de linha de comando é list-input-devices
    QCommandLineOption list_input_devices(
                QStringList() << "list-input-devices",
                QCoreApplication::translate("main",
                                            "Print a list input devices."));

//é definida a opção list_components onde o nome de opção de linha de comando é list-components
    QCommandLineOption list_components(
                QStringList() << "list-components",
                QCoreApplication::translate("main",
                                            "Print the list of components."));

// Adicionam opções de linha de comando que foram previamente definidas nateriormente e, após isso, processam tais opções 
    parser.addOption(list_backups_option);
    parser.addOption(list_input_devices);
    parser.addOption(list_components);
    parser.addOption(rollback_option);
    parser.addOption(start_option);
    parser.addOption(stop_option);
    parser.addOption(status_option);
    parser.addOption(debug_allow_empty_devices);
    parser.addOption(debug_allow_device_collisions);
    parser.addOption(debug_allow_missing_components);

// Analisa os argumentos passados pela linha de comando e verifica se as opções configuradas estão presentes
    parser.process(*a);

//Verifica se a opção de comando list_input_devices foi selecionada pelo usuário durante execução do programa
    if (parser.isSet(list_input_devices)) { //Verifica se a opção list_input_devices foi ativada. Em caso positivo, o usuário desejará listar os dispositivos de entrada
        for (QString dev : platform::get_input_devices()) {
            cout << dev.toStdString() << endl; // A cada iteração, o nome do dispositivo é impresso na saída padrão
        }
        return 0;
    }

    Configuration config; //variável config do tipo configuration é criada
    config.load(MST_CONFIG_FILE, MST_SEATS_CONFIG_FILE); //carrega as configurações do programa. Os argumentos são caminhos para arquivos de configuração.
    Template_manager::get_instance()->set_template_dir(MST_TEMPLATE_DIR); //o método define o diretório de modelos

//Se a opção debug_allow_missing_componentes tiver sido ativada, faz uma alteração na configuração do programa
    if (parser.isSet(debug_allow_missing_components)) {
        config.allow_missing_components(true);
    }

// Verifica se o programa está sendo executado com privilégio de superusuário (root). Em caso negativo, exibe mensagem de erro informando que o programa precisa ser executado em root.
    if (geteuid() != 0)
    {
        if (is_graphic_mode) {
            QMessageBox messageBox;
            messageBox.critical(
                        0,
                        QCoreApplication::translate("main",
                                                    "Error"),
                        QCoreApplication::translate(
                            "main",
                            "MST must be run as a superuser"));
        } else {
            cerr << QCoreApplication::translate(
                        "main",
                        "MST must be run as a superuser").toStdString()
                 << endl;
        }
        return 1;
    }

    MST* mst = MST::get_instance(); //criação de um ponteiro 'mst' para um objeto 'MST'. É obtida uma instância única da classe 'MST', permitindo que o objeto interaja com instância para controlar o sistema multiterminal
    mst->set_configuration(config); // Objeto 'config' é atribuído à instância 'mst'. Dessa forma, a configuração carregada será aplicada à instância do sistema multiterminal

    if (parser.isSet(status_option)) { //Verifica se a opção de linha de comando 'status_option' foi selecionada
        cout << "mstd: " << (mst->running_p() ? "running" : "stopped")
             << endl; // Exibe o status do sistema multiterminal. Se o sistema estiver em execução, exibirá "running", caso contrário, "stopped".
        int32_t seat_count = mst->get_seats().size(); // Obtém o número de assentos (seats) do sistema multiterminal.
        if (seat_count > 0) { // Verifica se há assentos no sistema.
            cout << seat_count << " seat(s):" << endl; // Exibe o número de assentos, seguido por "seat(s)".
            for (auto seat : mst->get_seats()) { // Itera sobre os assentos no sistema e exibe informações sobre cada um.
                cout << "  " << *seat << endl;
            }
        }
        return 0; // Retorna 0 para indicar que o programa foi executado com sucesso
    }

    if (parser.isSet(start_option)) { // Verifica se a opção de linha de comando 'start_option' foi selecionada.
        mst->start(); // Chama o método 'start()' na instância do sistema multiterminal 'mst' para iniciar o sistema.
        return 0; // Retorna 0 para indicar que o programa foi executado com sucesso.
    }

    if (parser.isSet(stop_option)) { // Verifica se a opção de linha de comando 'stop_option' foi selecionada.
        mst->stop(); // Chama o método 'stop()' na instância do sistema multiterminal 'mst' para parar o sistema.
        return 0; // Retorna 0 para indicar que o programa foi executado com sucesso.
    }

    if (parser.isSet(list_backups_option)) { // Verifica se a opção de linha de comando 'list_backups_option' foi selecionada.
        list_backups(mst); // Chama a função 'list_backups' passando a instância do sistema multiterminal 'mst' como argumento.
        return 0; // Retorna 0 para indicar que o programa foi executado com sucesso.
    }

    if (parser.isSet(list_components)) { // Verifica se a opção de linha de comando 'list_components' foi selecionada.
	cout << "xrandr:           " << PATH_TO_XRANDR << endl
	     << "pgrep:            " << PATH_TO_PGREP << endl
	     << "pkill:            " << PATH_TO_PKILL << endl
	     << "xset:             " << PATH_TO_XSET << endl
	     << "systemctl:        " << PATH_TO_SYSTEMCTL << endl
	     << "awesome:          " << PATH_TO_AWESOME << endl
	     << "lightdm:          " << PATH_TO_LIGHTDM << endl
	     << "dm-tool:          " << PATH_TO_DM_TOOL << endl
	     << "vgclient:         " << PATH_TO_VGLCLIENT << endl
	     << "vglserver_config: " << PATH_TO_VGLSERVER_CONFIG << endl
	     << "X:                " << PATH_TO_X << endl
	     << "Xephyr:           " << PATH_TO_XEPHYR << endl
	     << "bash:             " << PATH_TO_BASH << endl;
// Exibe os caminhos dos executáveis de diversos componentes.
	return 0; // Retorna 0 para indicar que o programa foi executado com sucesso.
    }

    if (parser.isSet(debug_allow_device_collisions)) { // Verifica se a opção de linha de comando 'debug_allow_device_collisions' foi selecionada.
        config.allow_device_collisions(true); // Chama um método na instância 'config' para permitir colisões de dispositivos.
    }

    if (parser.isSet(debug_allow_empty_devices)) { // Verifica se a opção de linha de comando 'debug_allow_empty_devices' foi selecionada
        config.allow_empty_devices(true); // Chama um método na instância 'config' para permitir dispositivos vazios.
    }

    if (parser.isSet(rollback_option)) { // Verifica se a opção de linha de comando 'rollback_option' foi selecionada.
        mst->disable(); // Chama o método 'disable()' na instância do sistema multiterminal 'mst'. Esse método provavelmente é responsável por desativar ou reverter as alterações feitas no sistema
        return 0; // Retorna 0 para indicar que o programa foi executado com sucesso.
    }

    if (is_graphic_mode) { // Verifica se o programa está sendo executado no modo gráfico.
        char* display = getenv("DISPLAY"); // Obtém o valor da variável de ambiente "DISPLAY", que geralmente contém informações sobre a tela gráfica atual.
        cerr << "Starting on display " << display << " ..." << endl; // Exibe uma mensagem informando qual display gráfico está sendo usado.
        InstallWindow w; // Cria uma instância da classe "InstallWindow" (presumivelmente uma janela gráfica).
        w.show(); // Exibe a janela gráfica.
        return a->exec(); // Inicia o loop de execução do aplicativo gráfico e aguarda até que o aplicativo seja encerrado. Após o encerramento, retorna o código de saída do aplicativo gráfico.
    } else { // Se o programa não estiver em modo gráfico (modo console).
        cerr << QCoreApplication::translate( // Exibe uma mensagem de erro informando que o programa não suporta o modo console.
                    "main",
                    "Currently MST does not support console mode.")
                .toStdString()
             << endl;
        return 1; // Retorna 1 para indicar que o programa foi executado com erro.
    }
}
