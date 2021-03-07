#ifndef ABOUTDIALOG_H
#define ABOUTDIALOG_H

#include <QDialog>
#include <QVBoxLayout>
#include <array>
#include <random>

class QString;
class QTextBrowser;

class AboutDialog : public QDialog {
    Q_OBJECT

public:
    AboutDialog(QWidget *parent = nullptr);

private:
    QVBoxLayout *all;
    QTextBrowser *authorsBrowser {};

    int prevTab;
};

#endif
