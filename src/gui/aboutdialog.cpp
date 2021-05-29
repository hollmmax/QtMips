#include "aboutdialog.h"

#include <QApplication>
#include <QDebug>
#include <QLabel>
#include <QObject>
#include <QPlainTextEdit>
#include <QPushButton>
#include <QTextBrowser>
#include <QVBoxLayout>

AboutDialog::AboutDialog(QWidget *parent) : QDialog(parent) {
    QLabel *lbl;

    setAttribute(Qt::WA_DeleteOnClose);
    setAttribute(Qt::WA_ShowModal);
    setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);

    setWindowTitle(tr("About Qt Mips"));

    all = new QVBoxLayout(this);

    QWidget *hbox = new QWidget();
    QHBoxLayout *hl = new QHBoxLayout(hbox);
    hl->setContentsMargins(0, 0, 0, 0);

    all->addWidget(hbox);

    QWidget *vbox = new QWidget();
    QVBoxLayout *vl = new QVBoxLayout(vbox);
    // vl->setContentsMargins(0,0,0,0);
    hl->addWidget(vbox);

    QString versionText;
    versionText = "Version " APP_VERSION "\n";

    vl->addWidget(new QLabel("<span style='font-size:x-large; font-weight:bold;"
                             "'>" APP_NAME " "
                             "- RISC-V Architecture Simulator</span>"));
    lbl = new QLabel(versionText);
    lbl->setAlignment(Qt::AlignHCenter);
    lbl->setOpenExternalLinks(true);
    vl->addWidget(lbl);
    vl->addWidget(new QLabel(
        "Copyright (C) 2017-2019 Karel Kočí "
        "<a href=\"mailto://cynerd@email.cz\">cynerd@email.cz</a><br/>"
        "Copyright (C) 2019 Pavel Píša "
        "<a "
        "href=\"mailto://pisa@cmp.felk.cvut.cz\">pisa@cmp.felk.cvut.cz</a>"));

    QString supportText;
    supportText = "Home Page : <a "
                  "href=\"" APP_GIT "\">" APP_GIT "</a><br/>"
                  "Implemented for<br/>"
                  "<a "
                  "href=\"https://cw.fel.cvut.cz/wiki/courses/b35apo/"
                  "start\">Computer Architectures</a><br/>"
                  "and<br/>"
                  "<a "
                  "href=\"https://cw.fel.cvut.cz/wiki/courses/b4m35pap/"
                  "start\">Advanced Computer Achitectures</a><br/>"
                  "courses at<br/>"
                  "<a href=\"https://www.cvut.cz/\">Czech Technical University "
                  "in Prague</a><br/>"
                  "<a href=\"https://www.fel.cvut.cz/\">Faculty of Electrical "
                  "Engineering</a><br/>";

    QTextBrowser *supportBrowser = new QTextBrowser;
    supportBrowser->setOpenExternalLinks(true);
    supportBrowser->setHtml(supportText);
    vl->addWidget(supportBrowser);

    QString licenseText;
    licenseText
        = "<p>This program is free software: you can redistribute it and/or "
          "modify it under the terms of the GNU General Public License "
          "as published by the Free Software Foundation, either version 3 of "
          "the License, or (at your option) any later version.</p>"
          "<p>This program is distributed in the hope that it will be "
          "useful, but WITHOUT ANY WARRANTY; without even the implied "
          "warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. "
          "See the GNU General Public License for more details.</p>"
          "<p>You should have received a copy of the GNU General Public "
          "License along with this program. If not, see <a href=\"https://www"
          ".gnu.org/licenses/\">https://www.gnu.org/licenses/</a>.</p>";

    QTextBrowser *licenseBrowser = new QTextBrowser;
    licenseBrowser->setOpenExternalLinks(true);
    licenseBrowser->setHtml(licenseText);
    vl->addWidget(licenseBrowser);

    QWidget *hbBtn = new QWidget();
    QHBoxLayout *hlBtn = new QHBoxLayout(hbBtn);
    hlBtn->setContentsMargins(0, 0, 0, 0);
    vl->addWidget(hbBtn);

    QPushButton *okButton = new QPushButton(tr("&OK"), parent);
    okButton->setFocus();
    connect(okButton, &QAbstractButton::clicked, this, &QWidget::close);
    hlBtn->addStretch();
    hlBtn->addWidget(okButton);

    setMinimumSize(450, 500);

    prevTab = 0; // first Tab is selected by default
}
