/*

 * Copyright (c) 2010 Daniel Hartmeier <daniel@benzedrine.cx>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 *    - Redistributions of source code must retain the above copyright
 *      notice, this list of conditions and the following disclaimer.
 *    - Redistributions in binary form must reproduce the above
 *      copyright notice, this list of conditions and the following
 *      disclaimer in the documentation and/or other materials provided
 *      with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
 * ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 *
 */

#ifndef __BOARD_H__
#define __BOARD_H__

#include <sys/types.h>
#include <utility>
#include <string>
#include <vector>

using namespace std;

enum RESULT { LOSS=0, UNDECIDED=1, DRAW=2, WIN=3 };

class Planet {
public:
	Planet(int id, int owner, int ships, int growth, double x, double y) :
	    id(id), owner(owner), ships(ships), growth(growth), x(x), y(y) {}
	int getId() const { return id; }
	int getOwner() const { return owner; }
	void setOwner(int owner) { this->owner = owner; }
	int getShips() const { return ships; }
	void setShips(int ships) { this->ships = ships; }
	int getGrowth() const { return growth; }
	double getX() const { return x; }
	double getY() const { return y; }
private:
	int id, owner, ships, growth;
	double x, y;
};

class Fleet {
public:
	Fleet(int owner, int ships, int src, int dst, int len, int remain) :
	    owner(owner), ships(ships), src(src), dst(dst), len(len),
	    remain(remain) {}
	int getOwner() const { return owner; }
	int getShips() const { return ships; }
	void setShips(int ships) { this->ships = ships; }
	int getSrc() const { return src; }
	int getDst() const { return dst; }
	int getLen() const { return len; }
	int getRemain() const { return remain; }
	int decrementRemain() { return --remain; }
private:
	int owner, ships, src, dst, len, remain;
};

class Board {
public:
	Board(const char *fn);
	bool isValid() const { return valid; }
	string getFn() const { return fn; }
	void print(int player, char *s, unsigned len) const;
	string describe() const;
	bool issueOrder(int player, int src, int dst, int ships, string &err);
	void nextTurn();
	int distance(int src, int dst) const;
	RESULT rate() const;
private:
	vector<Planet> planets;
	vector<Fleet> fleets;
	bool valid;
	string fn;
	unsigned turns;
};

#endif
